package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.local.TestHelper
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.{CoinbaseTransaction, EncryBaseTransaction, PaymentTransaction}
import encry.modifiers.state.box._
import encry.settings.{Algos, EncryAppSettings, NodeSettings}
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
          case tx@(_: PaymentTransaction | _: CoinbaseTransaction) =>
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
  val afterGenesisStateDigestHex: String = "BrXzqSBcUxvnM3YyhcNYXWnQBYoYRQGeWKVp8GpJChxGT"
  val afterGenesisStateDigest: ADDigest = ADDigest @@ Algos.decode(afterGenesisStateDigestHex).get

  lazy val genesisStateVersion: VersionTag = VersionTag @@ Algos.hash(afterGenesisStateDigest.tail)

  def stateDir(settings: EncryAppSettings) = new File(s"${settings.directory}/state")

  def indexDir(settings: EncryAppSettings) = new File(s"${settings.directory}/index")

  def genGenesisUtxoState(stateDir: File, indexDir: File,
                          nodeViewHolderRef: Option[ActorRef]): (UtxoState, BoxHolder) = {
    log.info("Generating genesis UTXO state.")

    lazy val initialBoxes: Seq[EncryBaseBox] = TestHelper.genAssetBoxes

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
    val stDir = stateDir(settings)
    stDir.mkdirs()

    val idxDir = indexDir(settings)
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
