package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool._
import encry.modifiers.state.box._
import encry.modifiers.state.box.proposition.HeightProposition
import encry.settings.{Algos, Constants, EncryAppSettings, NodeSettings}
import encry.view.history.Height
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
    EncryBoxStateChanges(
      txs.flatMap { tx =>
        tx.unlockers.map(u => Removal(u.boxId)) ++ tx.newBoxes.map(bx => Insertion(bx))
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
  val afterGenesisStateDigestHex: String = "RndNpwnYRCxR85QsnVAhRhbn5BjrsomEcZcRvXPuKT9SF"

  val afterGenesisStateDigest: ADDigest = ADDigest @@ Algos.decode(afterGenesisStateDigestHex).get

  lazy val genesisStateVersion: VersionTag = VersionTag @@ Algos.hash(afterGenesisStateDigest.tail)

  def getStateDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/state")

  // TODO: Generate CoinbaseBox'es at the genesis stage.
  def genesisBoxes: IndexedSeq[AssetBox] = {
    lazy val genesisSeed = Long.MaxValue
    lazy val rndGen = new scala.util.Random(genesisSeed)
    (0 until Constants.Chain.GenesisBoxesQty).map(_ =>
      AssetBox(HeightProposition(Height @@ -1), rndGen.nextLong(), Constants.Chain.GenesisBoxesAmount))
  }

  def generateGenesisUtxoState(stateDir: File,
                               nodeViewHolderRef: Option[ActorRef]): (UtxoState, BoxHolder) = {
    log.info("Generating genesis UTXO state.")

    lazy val initialBoxes: Seq[EncryBaseBox] = genesisBoxes

    val bh = BoxHolder(initialBoxes)

    UtxoState.fromBoxHolder(bh, stateDir, nodeViewHolderRef).ensuring(us => {
      log.debug(s"Expected afterGenesisDigest: $afterGenesisStateDigestHex")
      log.debug(s"Actual afterGenesisDigest:   ${Base58.encode(us.rootHash)}")
      log.info(s"Generated UTXO state with ${bh.boxes.size} boxes inside.")
      us.rootHash.sameElements(afterGenesisStateDigest) && us.version.sameElements(genesisStateVersion)
    }) -> bh
  }

  def generateGenesisDigestState(stateDir: File, settings: NodeSettings): DigestState = {
    DigestState.create(Some(genesisStateVersion), Some(afterGenesisStateDigest), stateDir, settings)
  }

  def readOrGenerate(settings: EncryAppSettings,
                     nodeViewHolderRef: Option[ActorRef]): EncryState[_] = {
    val stateDir = getStateDir(settings)
    stateDir.mkdirs()

    settings.nodeSettings.stateMode match {
      case StateMode.Digest => DigestState.create(None, None, stateDir, settings.nodeSettings)
      case StateMode.Utxo if stateDir.listFiles().nonEmpty => UtxoState.create(stateDir, nodeViewHolderRef)
      case _ => EncryState.generateGenesisUtxoState(stateDir, nodeViewHolderRef)._1
    }
  }
}
