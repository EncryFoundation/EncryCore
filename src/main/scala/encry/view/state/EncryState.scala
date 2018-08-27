package encry.view.state

import java.io.File
import akka.actor.ActorRef
import encry.VersionTag
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool._
import encry.modifiers.state.box._
import encry.settings.{Constants, EncryAppSettings, NodeSettings}
import encry.EncryApp.{system, settings}
import encry.stats.LoggingActor.LogMessage
import io.iohk.iodb.Store
import org.encryfoundation.common.utils.TaggedTypes.ADDigest
import scorex.crypto.encode.Base16
import scala.util.Try

trait EncryState[IState <: MinimalState[EncryPersistentModifier, IState]]
  extends MinimalState[EncryPersistentModifier, IState] {

  self: IState =>

  def rootHash: ADDigest

  val stateStore: Store

  def closeStorage(): Unit = stateStore.close()

  /** Extracts `state changes` from the given sequence of transactions. */
  def extractStateChanges(txs: Seq[Transaction]): EncryBoxStateChanges = {
    EncryBoxStateChanges(
      txs.flatMap { tx =>
        tx.inputs.map(u => Removal(u.boxId)) ++ tx.newBoxes.map(bx => Insertion(bx))
      }
    )
  }

  def extractStateChanges(tx: Transaction): EncryBoxStateChanges = extractStateChanges(Seq(tx))

  /** ID of the last applied modifier. */
  override def version: VersionTag

  override def applyModifier(mod: EncryPersistentModifier): Try[IState]

  override def rollbackTo(version: VersionTag): Try[IState]

  def rollbackVersions: Iterable[VersionTag]

  override type NVCT = this.type
}

object EncryState {

  def initialStateBoxes: IndexedSeq[AssetBox] = IndexedSeq(AssetBox(EncryProposition.open, -9, 0))

  val afterGenesisStateDigest: ADDigest = ADDigest @@ Base16.decode(Constants.AfterGenesisStateDigestHex)
    .getOrElse(throw new Error("Failed to decode genesis state digest"))

  val genesisStateVersion: VersionTag = VersionTag @@ Base16.decode(Constants.GenesisStateVersion)
    .getOrElse(throw new Error("Failed to decode genesis state digest"))

  def getStateDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/state")

  def generateGenesisUtxoState(stateDir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
    val supplyBoxes: List[EncryBaseBox] = EncryState.initialStateBoxes.toList
    UtxoState.genesis(supplyBoxes, stateDir, nodeViewHolderRef).ensuring(us => {
      if (settings.logging.enableLogging) {
        system.actorSelection("user/loggingActor") !
          LogMessage("Info", s"Expected afterGenesisDigest: ${Constants.AfterGenesisStateDigestHex}", System.currentTimeMillis())
        system.actorSelection("user/loggingActor") !
          LogMessage("Info", s"Actual afterGenesisDigest:   ${Base16.encode(us.rootHash)}", System.currentTimeMillis())
        system.actorSelection("user/loggingActor") !
          LogMessage("Info", s"Generated UTXO state with ${supplyBoxes.size} boxes inside.", System.currentTimeMillis())
      }
      us.rootHash.sameElements(afterGenesisStateDigest) && us.version.sameElements(genesisStateVersion)
    })
  }

  def generateGenesisDigestState(stateDir: File, settings: NodeSettings): DigestState =
    DigestState.create(Some(genesisStateVersion), Some(afterGenesisStateDigest), stateDir, settings)

  def readOrGenerate(settings: EncryAppSettings, nodeViewHolderRef: Option[ActorRef]): EncryState[_] = {
    val stateDir: File = getStateDir(settings)
    stateDir.mkdirs()
    settings.node.stateMode match {
      case StateMode.Digest => DigestState.create(None, None, stateDir, settings.node)
      case StateMode.Utxo if stateDir.listFiles().nonEmpty => UtxoState.create(stateDir, nodeViewHolderRef)
      case _ => EncryState.generateGenesisUtxoState(stateDir, nodeViewHolderRef)
    }
  }
}