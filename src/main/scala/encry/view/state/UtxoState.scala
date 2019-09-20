package encry.view.state

import java.io.File
import akka.actor.ActorRef
import com.google.common.primitives.{Ints, Longs}
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.EncrySupplyController
import encry.modifiers.state.{Context, EncryPropositionFunctions}
import encry.settings.{EncryAppSettings, LevelDBSettings}
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.BalanceCalculator
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.implicits.UTXO._
import encry.utils.implicits.Validation._
import encry.view.NodeViewErrors.ModifierApplyError
import encry.view.NodeViewErrors.ModifierApplyError.StateModifierApplyError
import io.iohk.iodb.LSMStore
import cats.data.Validated
import cats.Traverse
import cats.syntax.traverse._
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.validated._
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.StateModifierSerializer
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryBaseBox, EncryProposition}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, Height}
import org.encryfoundation.common.utils.constants.Constants
import org.encryfoundation.common.validation.ValidationResult.Invalid
import org.encryfoundation.common.validation.{MalformedModifierError, ValidationResult}
import org.iq80.leveldb.Options
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32
import scala.concurrent.ExecutionContextExecutor
import scala.util.Try

final case class UtxoState(storage: VersionalStorage, constants: Constants)
  extends StrictLogging with UtxoStateReader with AutoCloseable {

  var height: Height = Height @@ constants.PreGenesisHeight
  var lastBlockTimestamp: Long = 0L

  def applyModifier(mod: PersistentModifier): Either[List[ModifierApplyError], UtxoState] = {
    val startTime = System.currentTimeMillis()
    val result = mod match {
      case header: Header =>
        logger.info(s"\n\nStarting to applyModifier as a header: ${Algos.encode(mod.id)} to state at height ${header.height}")
        val newState: UtxoState = UtxoState(storage, constants)
        newState.height = height
        newState.lastBlockTimestamp = header.timestamp
        newState.asRight[List[ModifierApplyError]]
      case block: Block =>
        logger.info(s"\n\nStarting to applyModifier as a Block: ${Algos.encode(mod.id)} to state at height ${block.header.height}")
        val lastTxId = block.payload.txs.last.id
        val totalFees: Amount = block.payload.txs.init.map(_.fee).sum
        val validstartTime = System.currentTimeMillis()
        val res: Either[ValidationResult, List[Transaction]] = block.payload.txs.map(tx => {
          if (tx.id sameElements lastTxId) validate(tx, totalFees + EncrySupplyController.supplyAt(height,
            constants.InitialEmissionAmount, constants.EmissionEpochLength, constants.EmissionDecay))
          else validate(tx)
        }).toList
          .traverse(Validated.fromEither)
          .toEither
        logger.info(s"Validation time: ${(System.currentTimeMillis() - validstartTime)/1000L} s")
        res.fold(
          err => err.errors.map(modError => StateModifierApplyError(modError.message)).toList.asLeft[UtxoState],
          txsToApply => {
            val combineTimeStart = System.currentTimeMillis()
            val combinedStateChange = combineAll(txsToApply.map(UtxoState.tx2StateChange))
            logger.info(s"Time of combining: ${(System.currentTimeMillis() - combineTimeStart)/1000L} s")
            val insertTimestart = System.currentTimeMillis()
            storage.insert(
              StorageVersion !@@ block.id,
              combinedStateChange.outputsToDb.toList,
              combinedStateChange.inputsToDb.toList
            )
            logger.info(s"Time of insert: ${(System.currentTimeMillis() - insertTimestart)/1000L} s")
            val newState: UtxoState = UtxoState(storage, constants)
            newState.height = Height @@ block.header.height
            newState.lastBlockTimestamp = block.header.timestamp
            newState.asRight[List[ModifierApplyError]]
          }
        )
    }
    logger.info(s"Time of applying mod ${Algos.encode(mod.id)} of type ${mod.modifierTypeId} is (${(System.currentTimeMillis() - startTime)/1000L} s)")
    result
  }

  def rollbackTo(version: VersionTag): Try[UtxoState] = Try {
    storage.versions.find(_ sameElements version) match {
      case Some(_) =>
        logger.info(s"Rollback to version ${Algos.encode(version)}")
        storage.rollbackTo(StorageVersion !@@ version)
        val stateHeight: Int = storage.get(StorageKey @@ UtxoState.bestHeightKey.untag(Digest32))
          .map(d => Ints.fromByteArray(d)).getOrElse(constants.GenesisHeight)
        val stateNew = UtxoState(storage, constants)
        stateNew.height = Height @@ stateHeight
        stateNew
      case None => throw new Exception(s"Impossible to rollback to version ${Algos.encode(version)}")
    }
  }

  def validate(tx: Transaction, allowedOutputDelta: Amount = 0L): Either[ValidationResult, Transaction] =
    if (tx.semanticValidity.isSuccess) {
      val stateView: EncryStateView = EncryStateView(height, lastBlockTimestamp, ADDigest @@ Array.emptyByteArray)
      val bxs: IndexedSeq[EncryBaseBox] = tx.inputs.flatMap(input => storage.get(StorageKey !@@ input.boxId)
        .map(bytes => StateModifierSerializer.parseBytes(bytes, input.boxId.head))
        .map(_.toOption -> input))
        .foldLeft(IndexedSeq[EncryBaseBox]()) { case (acc, (bxOpt, input)) =>
          (bxOpt, tx.defaultProofOpt) match {
            // If no `proofs` provided, then `defaultProof` is used.
            case (Some(bx), defaultProofOpt) if input.proofs.nonEmpty =>
              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(tx, bx, stateView), input.contract,
                defaultProofOpt.map(input.proofs :+ _).getOrElse(input.proofs))) acc :+ bx else acc
            case (Some(bx), Some(defaultProof)) =>
              if (EncryPropositionFunctions.canUnlock(bx.proposition,
                Context(tx, bx, stateView), input.contract, Seq(defaultProof))) acc :+ bx else acc
            case (Some(bx), defaultProofOpt) =>
              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(tx, bx, stateView), input.contract,
                defaultProofOpt.map(Seq(_)).getOrElse(Seq.empty))) acc :+ bx else acc
            case _ => acc
          }
        }

      val validBalance: Boolean = {
        val debitB: Map[String, Amount] = BalanceCalculator.balanceSheet(bxs, constants.IntrinsicTokenId).map {
          case (key, value) => Algos.encode(key) -> value
        }
        val creditB: Map[String, Amount] = {
          val balanceSheet: Map[TokenId, Amount] =
            BalanceCalculator.balanceSheet(tx.newBoxes, constants.IntrinsicTokenId, excludeTokenIssuance = true)
          val intrinsicBalance: Amount = balanceSheet.getOrElse(constants.IntrinsicTokenId, 0L)
          balanceSheet.updated(constants.IntrinsicTokenId, intrinsicBalance + tx.fee)
        }.map { case (tokenId, amount) => Algos.encode(tokenId) -> amount }
        creditB.forall { case (tokenId, amount) =>
          if (tokenId == Algos.encode(constants.IntrinsicTokenId))
            debitB.getOrElse(tokenId, 0L) + allowedOutputDelta >= amount
          else debitB.getOrElse(tokenId, 0L) >= amount
        }
      }

      if (!validBalance) {
        logger.info(s"Tx: ${Algos.encode(tx.id)} invalid. Reason: Non-positive balance in $tx")
        Invalid(Seq(MalformedModifierError(s"Non-positive balance in $tx"))).asLeft[Transaction]
      }
      else if (bxs.length != tx.inputs.length) {
        logger.info(s"Tx: ${Algos.encode(tx.id)} invalid. Reason: Box not found")
        Invalid(Seq(MalformedModifierError(s"Box not found"))).asLeft[Transaction]
      }
      else tx.asRight[ValidationResult]
    } else tx.semanticValidity.errors.headOption
      .map(err => Invalid(Seq(err)).asLeft[Transaction])
      .getOrElse(tx.asRight[ValidationResult])

  def close(): Unit = storage.close()
}

object UtxoState extends StrictLogging {

  final case class StateChange(inputsToDb: Vector[StorageKey],
                               outputsToDb: Vector[(StorageKey, StorageValue)])

  private val bestVersionKey: Digest32 = Algos.hash("best_state_version")

  private val bestHeightKey: Digest32 = Algos.hash("state_height")

  private val lastBlockTimeKey: Digest32 = Algos.hash("last_block_timestamp")

  def tx2StateChange(tx: Transaction): StateChange = StateChange(
    tx.inputs.map(input => StorageKey !@@ input.boxId).toVector,
    tx.newBoxes.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes)).toVector
  )

  def initialStateBoxes: List[AssetBox] = List(AssetBox(EncryProposition.open, -9, 0))

  def getStateDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/state")

  def create(stateDir: File,
             nodeViewHolderRef: Option[ActorRef],
             settings: EncryAppSettings,
             statsSenderRef: Option[ActorRef]): UtxoState = {
    val versionalStorage = settings.storage.state match {
      case VersionalStorage.IODB =>
        logger.info("Init state with iodb storage")
        IODBWrapper(new LSMStore(stateDir, keepVersions = settings.constants.DefaultKeepVersions))
      case VersionalStorage.LevelDB =>
        logger.info("Init state with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(stateDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(300, 32), keySize = 32))
    }
    val stateHeight: Int = versionalStorage.get(StorageKey @@ bestHeightKey.untag(Digest32))
      .map(d => Ints.fromByteArray(d)).getOrElse(settings.constants.PreGenesisHeight)
    val lastBlockTimestamp: Amount = versionalStorage.get(StorageKey @@ lastBlockTimeKey.untag(Digest32))
      .map(d => Longs.fromByteArray(d)).getOrElse(0L)
    val state = new UtxoState(versionalStorage, settings.constants)
    state.height = Height @@ stateHeight
    state.lastBlockTimestamp = lastBlockTimestamp
    state
  }

  def genesis(stateDir: File,
              nodeViewHolderRef: Option[ActorRef],
              settings: EncryAppSettings,
              statsSenderRef: Option[ActorRef]): UtxoState = {
    //check kind of storage
    val storage = settings.storage.state match {
      case VersionalStorage.IODB =>
        logger.info("Init state with iodb storage")
        IODBWrapper(new LSMStore(stateDir, keepVersions = settings.constants.DefaultKeepVersions))
      case VersionalStorage.LevelDB =>
        logger.info("Init state with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(stateDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }

    storage.insert(
      StorageVersion @@ Array.fill(32)(0: Byte),
      initialStateBoxes.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))
    )
    UtxoState(storage, settings.constants)
  }
}