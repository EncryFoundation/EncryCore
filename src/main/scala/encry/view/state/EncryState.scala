package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.consensus.emission.EncrySupplyController
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool._
import encry.modifiers.state.box._
import encry.modifiers.state.box.proposition.HeightProposition
import encry.settings.{Constants, EncryAppSettings, NodeSettings}
import encry.view.history.Height
import io.iohk.iodb.Store
import scorex.core.VersionTag
import scorex.core.transaction.state.MinimalState
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.encode.Base16

import scala.util.{Random, Try}

trait EncryState[IState <: MinimalState[EncryPersistentModifier, IState]]
  extends MinimalState[EncryPersistentModifier, IState] with ScorexLogging {

  self: IState =>

  def rootHash: ADDigest

  val stateStore: Store

  def closeStorage(): Unit = stateStore.close()

  /** Extracts `state changes` from the given sequence of transactions. */
  def extractStateChanges(txs: Seq[EncryBaseTransaction]): EncryBoxStateChanges = {
    EncryBoxStateChanges(
      txs.flatMap { tx =>
        tx.unlockers.map(u => Removal(u.boxId)) ++ tx.newBoxes.map(bx => Insertion(bx))
      }
    )
  }

  def extractStateChanges(tx: EncryBaseTransaction): EncryBoxStateChanges = extractStateChanges(Seq(tx))

  /** ID of the last applied modifier. */
  override def version: VersionTag

  override def applyModifier(mod: EncryPersistentModifier): Try[IState]

  override def rollbackTo(version: VersionTag): Try[IState]

  def rollbackVersions: Iterable[VersionTag]

  override type NVCT = this.type
}

object EncryState extends ScorexLogging {

  val afterGenesisStateDigest: ADDigest = ADDigest @@ Base16.decode(Constants.AfterGenesisStateDigestHex)
    .getOrElse(throw new Error("Failed to decode genesis state diegst"))

  val genesisStateVersion: VersionTag = VersionTag @@ Array.fill(32)(9: Byte)

  def getStateDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/state")

  def totalSupplyBoxes: Seq[AssetBox] = {
    lazy val genesisSeed: Long = Long.MaxValue
    lazy val rndGen: Random = new scala.util.Random(genesisSeed)
    (0 until Constants.Chain.EmissionEpochLength).map { i =>
      AssetBox(HeightProposition(Height @@ i), rndGen.nextLong(), EncrySupplyController.supplyAt(Height @@ i))
    }
  }

  def generateGenesisUtxoState(stateDir: File,
                               nodeViewHolderRef: Option[ActorRef]): (UtxoState, BoxHolder) = {
    log.info("Generating genesis UTXO state.")

    val initialBoxes: Seq[EncryBaseBox] = totalSupplyBoxes

    val boxHolder: BoxHolder = BoxHolder(initialBoxes)

    UtxoState.fromBoxHolder(boxHolder, stateDir, nodeViewHolderRef).ensuring(us => {
      log.debug(s"Expected afterGenesisDigest: ${Constants.AfterGenesisStateDigestHex}")
      log.debug(s"Actual afterGenesisDigest:   ${Base16.encode(us.rootHash)}")
      log.info(s"Generated UTXO state with ${boxHolder.boxes.size} boxes inside.")
      us.rootHash.sameElements(afterGenesisStateDigest) && us.version.sameElements(genesisStateVersion)
    }) -> boxHolder
  }

  def generateGenesisDigestState(stateDir: File, settings: NodeSettings): DigestState = {
    DigestState.create(Some(genesisStateVersion), Some(afterGenesisStateDigest), stateDir, settings)
  }

  def readOrGenerate(settings: EncryAppSettings,
                     nodeViewHolderRef: Option[ActorRef]): EncryState[_] = {
    val stateDir: File = getStateDir(settings)
    stateDir.mkdirs()

    settings.nodeSettings.stateMode match {
      case StateMode.Digest => DigestState.create(None, None, stateDir, settings.nodeSettings)
      case StateMode.Utxo if stateDir.listFiles().nonEmpty => UtxoState.create(stateDir, nodeViewHolderRef)
      case _ => EncryState.generateGenesisUtxoState(stateDir, nodeViewHolderRef)._1
    }
  }
}
