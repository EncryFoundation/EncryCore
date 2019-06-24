package encry.view.state

import java.io.File

import akka.actor.ActorRef
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.state.{Context, EncryPropositionFunctions}
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.utils.BalanceCalculator
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.implicits.Validation._
import encry.utils.implicits.UTXO._
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.modifiers.mempool.transaction.{Input, Transaction}
import org.encryfoundation.common.modifiers.state.StateModifierSerializer
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.EncryBaseBox
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ADKey, Height}
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.{MalformedModifierError, ValidationResult}
import org.encryfoundation.common.validation.ValidationResult.{Invalid, Valid}
import cats.data.Validated
import cats.syntax.validated._
import cats.syntax.either._
import cats.syntax.traverse._
import cats.instances.list._
import cats.Traverse
import com.google.common.primitives.Ints
import encry.avltree.VersionedAVLStorage
import encry.consensus.EncrySupplyController
import encry.settings.{EncryAppSettings, LevelDBSettings}
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import io.iohk.iodb.LSMStore
import org.iq80.leveldb.Options
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Try}

final case class UtxoStateWithoutAVL(storage: VersionalStorage,
                                     height: Height,
                                     lastBlockTimestamp: Long) extends MinimalState[PersistentModifier, UtxoStateWithoutAVL] with StrictLogging {

  override type NVCT = this.type

  override def applyModifier(mod: PersistentModifier): Try[UtxoStateWithoutAVL] = mod match {
    case header: Header =>
      logger.info(s"\n\nStarting to applyModifier as a Block: ${Algos.encode(mod.id)} to state at height")
      Try (
        UtxoStateWithoutAVL(
          storage,
          Height @@ header.height,
          header.timestamp
        )
      )
    case block: Block =>
      logger.info(s"\n\nStarting to applyModifier as a Block: ${Algos.encode(mod.id)} to state at height")
      val lastTxId = block.payload.txs.last.id
      val totalFees: Amount = block.payload.txs.init.map(_.fee).sum
      val res: Either[ValidationResult, List[Transaction]] = block.payload.txs.map(tx => {
        if (tx.id sameElements lastTxId) validate(tx, totalFees + EncrySupplyController.supplyAt(height))
        else validate(tx)
      }).toList
        .traverse(Validated.fromEither)
        .toEither
      res.fold(
        err => {
          logger.info(s"Error during applying block ${block.encodedId}. Errors: ${err.errors.map(_.message).mkString(",")}")
          Try(this)
        },
        txsToApply => {
          val combinedStateChange = combineAll(txsToApply.map(UtxoStateWithoutAVL.tx2StateChange))
          storage.insert(
            StorageVersion !@@ block.id,
            combinedStateChange.outputsToDb,
            combinedStateChange.inputsToDb,
          )
          Try (
            UtxoStateWithoutAVL(
              storage,
              Height @@ block.header.height,
              block.header.timestamp
            )
          )
        }
      )
  }

  override def rollbackTo(version: VersionTag): Try[UtxoStateWithoutAVL] = Try {
    storage.versions.find(_ sameElements version) match {
      case Some(v) =>
        logger.info(s"Rollback to version ${Algos.encode(version)}")
        storage.rollbackTo(StorageVersion !@@ version)
        val stateHeight: Int = storage.get(StorageKey @@ UtxoStateWithoutAVL.bestHeightKey.untag(Digest32))
          .map(d => Ints.fromByteArray(d)).getOrElse(TestNetConstants.GenesisHeight)
        UtxoStateWithoutAVL(
          storage,
          Height @@ stateHeight,
          lastBlockTimestamp
        )
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
        val debitB: Map[String, Amount] = BalanceCalculator.balanceSheet(bxs).map {
          case (key, value) => Algos.encode(key) -> value
        }
        val creditB: Map[String, Amount] = {
          val balanceSheet: Map[TokenId, Amount] =
            BalanceCalculator.balanceSheet(tx.newBoxes, excludeTokenIssuance = true)
          val intrinsicBalance: Amount = balanceSheet.getOrElse(TestNetConstants.IntrinsicTokenId, 0L)
          balanceSheet.updated(TestNetConstants.IntrinsicTokenId, intrinsicBalance + tx.fee)
        }.map { case (tokenId, amount) => Algos.encode(tokenId) -> amount }
        creditB.forall { case (tokenId, amount) =>
          if (tokenId == Algos.encode(TestNetConstants.IntrinsicTokenId))
            debitB.getOrElse(tokenId, 0L) + allowedOutputDelta >= amount
          else debitB.getOrElse(tokenId, 0L) >= amount
        }
      }

      if (!validBalance) {
        logger.info(s"Tx: ${Algos.encode(tx.id)} invalid. Reason: Non-positive balance in $tx")
        Left[Invalid, Transaction](Invalid(Seq(MalformedModifierError(s"Non-positive balance in $tx"))))
      }
      else if (bxs.length != tx.inputs.length) {
        logger.info(s"Tx: ${Algos.encode(tx.id)} invalid. Reason: Box not found")
        Left[Invalid, Transaction](Invalid(Seq(MalformedModifierError(s"Box not found"))))
      }
      else Right[Invalid, Transaction](tx)
    } else tx.semanticValidity.errors.headOption.map(err => Left[Invalid, Transaction](Invalid(Seq(err)))).getOrElse(Right[Invalid, Transaction](tx))

  override def version: VersionTag = VersionTag @@ Array.emptyByteArray

  def closeStorage(): Unit = storage.close()
}

object UtxoStateWithoutAVL extends StrictLogging {

  final case class StateChange(inputsToDb: List[StorageKey],
                               outputsToDb: List[(StorageKey, StorageValue)])

  private val bestVersionKey: Digest32 = Algos.hash("best_state_version")

  private val bestHeightKey: Digest32 = Algos.hash("state_height")

  private val lastBlockTimeKey: Digest32 = Algos.hash("last_block_timestamp")

  def tx2StateChange(tx: Transaction): StateChange = StateChange(
    tx.inputs.map(input => StorageKey !@@ input.boxId).toList,
    tx.newBoxes.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes)).toList
  )

  def genesis(boxes: List[EncryBaseBox],
              stateDir: File,
              nodeViewHolderRef: Option[ActorRef],
              settings: EncryAppSettings,
              statsSenderRef: Option[ActorRef]): UtxoStateWithoutAVL = {
    //check kind of storage
    val storage = settings.storage.state match {
      case VersionalStorage.IODB =>
        logger.info("Init state with iodb storage")
        IODBWrapper(new LSMStore(stateDir, keepVersions = TestNetConstants.DefaultKeepVersions))
      case VersionalStorage.LevelDB =>
        logger.info("Init state with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(stateDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }

    storage.insert(
      StorageVersion @@ Array.fill(32)(0: Byte),
      boxes.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))
    )

    new UtxoStateWithoutAVL(
      storage,
      TestNetConstants.PreGenesisHeight,
      0L,
    )
  }
}
