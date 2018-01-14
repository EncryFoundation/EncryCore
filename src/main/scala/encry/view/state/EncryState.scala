package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.crypto.Address
import encry.local.TransactionFactory
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.{CoinbaseTransaction, EncryBaseTransaction, PaymentTransaction}
import encry.modifiers.state.box._
import encry.modifiers.state.box.proposition.AddressProposition
import encry.settings.{Algos, EncryAppSettings, NodeSettings}
import scorex.core.VersionTag
import scorex.core.transaction.state.MinimalState
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.encode.{Base16, Base58}

import scala.util.Try

trait EncryState[IState <: MinimalState[EncryPersistentModifier, IState]]
  extends MinimalState[EncryPersistentModifier, IState] with ScorexLogging {

  self: IState =>

  def rootHash(): ADDigest

  // TODO: Implement correctly.
  def stateHeight(): Int = 0

  // TODO: Do we need tx matching here?
  // Extracts `state changes` from the given sequence of transactions.
  def getAllStateChanges(txs: Seq[EncryBaseTransaction]): EncryBoxStateChanges = {
    // Use neither `.filter` nor any validity checks here!
    // This method should be invoked when all txs are already validated.
    EncryBoxStateChanges(
      txs.flatMap { tx =>
        tx match {
          case tx: PaymentTransaction =>
            tx.unlockers.map(unl => Removal(unl.closedBoxId)) ++
              tx.newBoxes.map(bx => Insertion(bx))
          case tx: CoinbaseTransaction =>
            tx.unlockers.map(unl => Removal(unl.closedBoxId)) ++
              tx.newBoxes.map(bx => Insertion(bx))
        }
      }
    )
  }

  def getStateChanges(tx: EncryBaseTransaction): EncryBoxStateChanges = getAllStateChanges(Seq(tx))

// TODO: Implement:  def boxesOf(proposition: Proposition): Seq[Box[proposition.type]]

  // ID of last applied modifier.
  override def version: VersionTag

  override def applyModifier(mod: EncryPersistentModifier): Try[IState]

  override def rollbackTo(version: VersionTag): Try[IState]

  def rollbackVersions: Iterable[VersionTag]

  override type NVCT = this.type

}

object EncryState extends ScorexLogging{

  // 33 bytes in Base58 encoding.
  val afterGenesisStateDigestHex: String = "Tgx54rhrGNhTnrHBiRYwyugnCo7A61ZR4xSq2xcqAv9kG"
  val afterGenesisStateDigest: ADDigest = ADDigest @@ Algos.decode(afterGenesisStateDigestHex).get

  lazy val genesisStateVersion: VersionTag = VersionTag @@ Algos.hash(afterGenesisStateDigest.tail)

  def stateDir(settings: EncryAppSettings) = new File(s"${settings.directory}/state")

  def genGenesisUtxoState(stateDir: File, nodeViewHolderRef: Option[ActorRef]): (UtxoState, BoxHolder) = {
    log.info("Generating genesis UTXO state.")
    lazy val genesisSeed = Long.MaxValue

    lazy val initialBoxes: Seq[EncryBaseBox] = TransactionFactory.genAssetBoxes

    val bh = BoxHolder(initialBoxes)

    UtxoState.fromBoxHolder(bh, stateDir, nodeViewHolderRef).ensuring(us => {
      log.info(s"Generated UTXO state with ${bh.boxes.size} boxes inside.")
      us.rootHash.sameElements(afterGenesisStateDigest) && us.version.sameElements(genesisStateVersion)
    }) -> bh
  }

  def generateGenesisDigestState(stateDir: File, settings: NodeSettings): DigestState = {
    DigestState.create(Some(genesisStateVersion), Some(afterGenesisStateDigest), stateDir, settings).get //todo: .get
  }

  // TODO:
  def readOrGenerate(settings: EncryAppSettings, nodeViewHolderRef: Option[ActorRef]): Option[EncryState[_]] = {
    val dir = stateDir(settings)
    dir.mkdirs()

    if (dir.listFiles().isEmpty) {
      None
    } else {
      //todo: considering db state
      if (settings.nodeSettings.ADState) DigestState.create(None, None, dir, settings.nodeSettings).toOption
      else Some(UtxoState.create(dir, nodeViewHolderRef))
    }
  }
}