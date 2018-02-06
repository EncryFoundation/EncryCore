package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.local.TestHelper
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.{AddPubKeyInfoTransaction, CoinbaseTransaction, EncryBaseTransaction, PaymentTransaction}
import encry.modifiers.state.box._
import encry.modifiers.state.box.proposition.{AddressProposition, HeightProposition}
import encry.settings.{Algos, EncryAppSettings, NodeSettings}
import encry.view.history.Height
import encry.view.state.index.StateIndexManager
import scorex.core.VersionTag
import scorex.core.transaction.state.MinimalState
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.encode.Base58

import scala.util.Try

trait EncryState[IState <: MinimalState[EncryPersistentModifier, IState]]
  extends MinimalState[EncryPersistentModifier, IState] with ScorexLogging {

  self: IState =>

  def rootHash(): ADDigest

  // Extracts `state changes` from the given sequence of transactions.
  def getAllStateChanges(txs: Seq[EncryBaseTransaction]): EncryBoxStateChanges = {
    // Use neither `.filter` nor any validity checks here!
    // This method should be invoked when all txs are already validated.
    EncryBoxStateChanges(
      txs.flatMap { tx =>
        tx match {
          case tx @ (_: PaymentTransaction | _: CoinbaseTransaction | _: AddPubKeyInfoTransaction) =>
            tx.useBoxes.map(bxId => Removal(bxId)) ++
              tx.newBoxes.map(bx => Insertion(bx))
        }
      }
    )
  }

  def getStateChanges(tx: EncryBaseTransaction): EncryBoxStateChanges = getAllStateChanges(Seq(tx))

  // ID of last applied modifier.
  override def version: VersionTag

  override def applyModifier(mod: EncryPersistentModifier): Try[IState]

  override def rollbackTo(version: VersionTag): Try[IState]

  def rollbackVersions: Iterable[VersionTag]

  override type NVCT = this.type
}

object EncryState extends ScorexLogging{

  // 33 bytes in Base58 encoding.
  val afterGenesisStateDigestHex: String = "29KLRhyQqxtFT5y6MQGwLJ3iqbdCziSBudBvH5aCJxQcYk"

  val afterGenesisStateDigest: ADDigest = ADDigest @@ Algos.decode(afterGenesisStateDigestHex).get

  lazy val genesisStateVersion: VersionTag = VersionTag @@ Algos.hash(afterGenesisStateDigest.tail)

  def getStateDir(settings: EncryAppSettings) = new File(s"${settings.directory}/state")

  def getIndexDir(settings: EncryAppSettings) = new File(s"${settings.directory}/index")

  // TODO: Magic numbers. Move to settings.
  def initialOpenBoxes: IndexedSeq[OpenBox] = (0 until 100).map(i =>
    OpenBox(HeightProposition(Height @@ -1), 9999 * i, 20L))

  def genesisEmptyBoxes: IndexedSeq[AssetBox] = {
    lazy val genesisSeed = Long.MaxValue
    lazy val rndGen = new scala.util.Random(genesisSeed)
    (0 until 20).map(_ =>
      AssetBox(AddressProposition(StateIndexManager.openBoxesAddress), rndGen.nextLong(), 0L))
  }

  def generateGenesisUtxoState(stateDir: File, indexDir: File,
                               nodeViewHolderRef: Option[ActorRef]): (UtxoState, BoxHolder) = {
    log.info("Generating genesis UTXO state.")

    lazy val initialBoxes: Seq[EncryBaseBox] = genesisEmptyBoxes

    val bh = BoxHolder(initialBoxes)

    UtxoState.fromBoxHolder(bh, stateDir, indexDir, nodeViewHolderRef).ensuring(us => {
      log.debug(s"Expected afterGenesisDigest: $afterGenesisStateDigestHex")
      log.debug(s"Actual afterGenesisDigest:   ${Base58.encode(us.rootHash)}")
      log.info(s"Generated UTXO state with ${bh.boxes.size} boxes inside.")
      us.rootHash.sameElements(afterGenesisStateDigest) && us.version.sameElements(genesisStateVersion)
    }) -> bh
  }

  def generateGenesisDigestState(stateDir: File, settings: NodeSettings): DigestState = {
    DigestState.create(Some(genesisStateVersion), Some(afterGenesisStateDigest), stateDir, settings).get //todo: .get
  }

  // TODO:
  def readOrGenerate(settings: EncryAppSettings,
                     nodeViewHolderRef: Option[ActorRef]): Option[EncryState[_]] = {
    val stDir = getStateDir(settings)
    stDir.mkdirs()

    val idxDir = getIndexDir(settings)
    idxDir.mkdirs()

    if (stDir.listFiles().isEmpty) {
      None
    } else {
      //todo: considering db state
      if (settings.nodeSettings.ADState) DigestState.create(None, None, stDir, settings.nodeSettings).toOption
      else Some(UtxoState.create(stDir, idxDir, nodeViewHolderRef))
    }
  }
}
