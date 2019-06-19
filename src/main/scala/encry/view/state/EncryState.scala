package encry.view.state

import java.io.File

import akka.actor.ActorRef
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage
import encry.utils.CoreTaggedTypes.VersionTag
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.box._
import org.encryfoundation.common.utils.TaggedTypes.ADDigest
import org.encryfoundation.common.utils.constants.TestNetConstants
import scorex.crypto.encode.Base16
import scala.util.Try

trait EncryState[IState <: MinimalState[PersistentModifier, IState]]
  extends MinimalState[PersistentModifier, IState] {

  self: IState =>

  def rootHash: ADDigest

  val stateStore: VersionalStorage

  def closeStorage(): Unit = stateStore.close()

  /** Extracts `state changes` from the given sequence of transactions. */
  def extractStateChanges(txs: Seq[Transaction]): EncryBoxStateChanges = {
    EncryBoxStateChanges(txs.flatMap { tx =>
      tx.inputs.map(u => Removal(u.boxId)) ++ tx.newBoxes.map(bx => Insertion(bx))
    }
    )
  }

  def extractStateChanges(tx: Transaction): EncryBoxStateChanges = extractStateChanges(Seq(tx))

  /** ID of the last applied modifier. */
  override def version: VersionTag

  override def applyModifier(mod: PersistentModifier, lastHeaderHeight: Int): Try[IState]

  override def rollbackTo(version: VersionTag): Try[IState]

  def rollbackVersions: Iterable[VersionTag]

  override type NVCT = this.type
}

object EncryState extends StrictLogging {

  def initialStateBoxes: IndexedSeq[AssetBox] = IndexedSeq(AssetBox(EncryProposition.open, -9, 0))

  val afterGenesisStateDigest: ADDigest = ADDigest @@ Base16.decode(TestNetConstants.AfterGenesisStateDigestHex)
    .getOrElse(throw new Error("Failed to decode genesis state digest"))

  val genesisStateVersion: VersionTag = VersionTag @@ Base16.decode(TestNetConstants.GenesisStateVersion)
    .getOrElse(throw new Error("Failed to decode genesis state digest"))

  def getStateDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/state")

  def generateGenesisUtxoState(stateDir: File,
                               nodeViewHolderRef: Option[ActorRef],
                               settings: EncryAppSettings,
                               statsSenderRef: Option[ActorRef]): UtxoState = {
    val supplyBoxes: List[EncryBaseBox] = EncryState.initialStateBoxes.toList
    UtxoState.genesis(supplyBoxes, stateDir, nodeViewHolderRef, settings, statsSenderRef).ensuring(us => {
      logger.info(s"Expected afterGenesisDigest: ${TestNetConstants.AfterGenesisStateDigestHex}")
      logger.info(s"Actual afterGenesisDigest:   ${Base16.encode(us.rootHash)}")
      logger.info(s"Generated UTXO state with ${supplyBoxes.size} boxes inside.")
      us.rootHash.sameElements(afterGenesisStateDigest) && us.version.sameElements(genesisStateVersion)
    })
  }

  def generateGenesisDigestState(stateDir: File, settings: EncryAppSettings): DigestState =
    DigestState.create(Some(genesisStateVersion), Some(afterGenesisStateDigest), stateDir, settings)

  def readOrGenerate(settings: EncryAppSettings,
                     nodeViewHolderRef: Option[ActorRef],
                     statsSenderRef: Option[ActorRef]): EncryState[_] = {
    val stateDir: File = getStateDir(settings)
    stateDir.mkdirs()
    settings.node.stateMode match {
      case StateMode.Digest => DigestState.create(None, None, stateDir, settings)
      case StateMode.Utxo if stateDir.listFiles().nonEmpty =>
        UtxoState.create(stateDir, nodeViewHolderRef, settings, statsSenderRef)
      case _ => EncryState.generateGenesisUtxoState(stateDir, nodeViewHolderRef, settings, statsSenderRef)
    }
  }
}