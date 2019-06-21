package encry.view.state

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.state.{Context, EncryPropositionFunctions}
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.utils.BalanceCalculator
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.implicits.Validation._
import encry.view.state.UtxoStateWithoutAVL._
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

import scala.util.Try

final case class UtxoStateWithoutAVL(storage: VersionalStorage,
                                     height: Height,
                                     lastBlockTimestamp: Long) extends MinimalState[PersistentModifier, UtxoStateWithoutAVL] with StrictLogging {

  override def applyModifier(mod: PersistentModifier): Try[UtxoStateWithoutAVL] = mod match {
    case header: Header => Try(this)
    case block: Block =>
      logger.info(s"\n\nStarting to applyModifier as a Block: ${Algos.encode(mod.id)} to state at height")
      val res: Either[ValidationResult, List[Transaction]] = block.payload.txs.map(tx => validate(tx)).toList
        .traverse(Validated.fromEither)
        .toEither
      res.fold(
        err => {
          logger.info(s"Error during applying block ${block.encodedId}. Errors: ${err.errors.map(_.message).mkString(",")}")
          Try(this)
        },
        _ => Try(this)
      )
  }

  override def rollbackTo(version: VersionTag): Try[UtxoStateWithoutAVL] = ???

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

  override def version: VersionTag = ???

  override type NVCT = this.type
}

object UtxoStateWithoutAVL {

  final case class StateChange(inputsToDb: List[StorageKey],
                               outputsToDb: List[(StorageKey, StorageValue)])

  def tx2StateChange(tx: Transaction): StateChange = StateChange(
    tx.inputs.map(input => StorageKey !@@ input.boxId).toList,
    tx.newBoxes.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes)).toList
  )
}
