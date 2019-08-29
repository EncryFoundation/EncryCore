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
import cats.{Applicative, Functor, Monad, Traverse}
import cats.syntax.traverse._
import cats.instances.list._
import cats.syntax._
import cats.kernel.Monoid
import cats.syntax.either._
import cats.syntax.validated._
import monix.eval.{Callback, Task}
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.modifiers.mempool.transaction.{Input, Transaction}
import org.encryfoundation.common.modifiers.state.StateModifierSerializer
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryBaseBox, EncryProposition}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, Height}
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.ValidationResult.{Invalid, Valid}
import org.encryfoundation.common.validation.{MalformedModifierError, ValidationResult}
import org.iq80.leveldb.Options
import scorex.crypto.hash.Digest32
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.language.higherKinds
import scala.util.Try

final case class UtxoState(storage: VersionalStorage,
                           height: Height,
                           lastBlockTimestamp: Long)
  extends StrictLogging with UtxoStateReader with AutoCloseable {

  def validateTransaction(transaction: Transaction,
                          allowedOutputDelta: Amount = 0): Future[Either[ValidationResult, Transaction]] =
    if (transaction.semanticValidity.isSuccess) {
      val stateView: EncryStateView = EncryStateView(height, lastBlockTimestamp)
      val getFromDBAndParse: Future[IndexedSeq[(EncryBaseBox, Input)]] = Future.sequence(
        transaction.inputs.map { input =>
          logger.info(s"Started future for box in transaction ${transaction.encodedId}, ${Algos.encode(input.boxId)}")
          Future(storage
            .get(StorageKey !@@ input.boxId)
            .flatMap(bytes => StateModifierSerializer.parseBytes(bytes, input.boxId.head).toOption)
            .map(_ -> input))
        })
        .map(_.flatten)

      val validateContracts: Future[List[EncryBaseBox]] = getFromDBAndParse
        .map(_.foldLeft(List.empty[EncryBaseBox]) { case (acc, (bxOpt, input)) =>
          logger.info(s"Execute task 2 for input ${Algos.encode(input.boxId)}")
          (bxOpt, transaction.defaultProofOpt) match {
            // If no `proofs` provided, then `defaultProof` is used.
            case (bx, defaultProofOpt) if input.proofs.nonEmpty =>
              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(transaction, bx, stateView), input.contract,
                defaultProofOpt.map(input.proofs :+ _).getOrElse(input.proofs))) bx :: acc else acc
            case (bx, Some(defaultProof)) =>
              if (EncryPropositionFunctions.canUnlock(bx.proposition,
                Context(transaction, bx, stateView), input.contract, Seq(defaultProof))) bx :: acc else acc
            case (bx, defaultProofOpt) =>
              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(transaction, bx, stateView), input.contract,
                defaultProofOpt.map(Seq(_)).getOrElse(Seq.empty))) bx :: acc else acc
            case _ => acc
          }
        })
      val validateDebitAndCredit: Future[Boolean] = validateContracts.map { boxes =>
        val debitB: Map[String, Amount] = BalanceCalculator.balanceSheet(boxes).map {
          case (key, value) => Algos.encode(key) -> value
        }
        val creditB: Map[String, Amount] = {
          val balanceSheet: Map[TokenId, Amount] =
            BalanceCalculator.balanceSheet(transaction.newBoxes, excludeTokenIssuance = true)
          val intrinsicBalance: Amount = balanceSheet.getOrElse(TestNetConstants.IntrinsicTokenId, 0L)
          balanceSheet.updated(TestNetConstants.IntrinsicTokenId, intrinsicBalance + transaction.fee)
        }.map { case (tokenId, amount) => Algos.encode(tokenId) -> amount }
        creditB.forall { case (tokenId, amount) =>
          if (tokenId == Algos.encode(TestNetConstants.IntrinsicTokenId))
            debitB.getOrElse(tokenId, 0L) + allowedOutputDelta >= amount
          else debitB.getOrElse(tokenId, 0L) >= amount
        }
      }

      val result: Future[Either[ValidationResult, Transaction]] = validateDebitAndCredit.map { res =>
        if (!res) {
          logger.info(s"Tx: ${Algos.encode(transaction.id)} invalid. Reason: Non-positive balance in $transaction")
          Invalid(Seq(MalformedModifierError(s"Non-positive balance in $transaction"))).asLeft[Transaction]
        } else transaction.asRight[ValidationResult]
      }

      result
    } else Future.successful(transaction.semanticValidity.errors.headOption
      .map(err => Invalid(Seq(err)).asLeft[Transaction])
      .getOrElse(transaction.asRight[ValidationResult]))

  def validate(tx: Transaction, allowedOutputDelta: Amount = 0L): Task[Either[ValidationResult, Transaction]] =
    if (tx.semanticValidity.isSuccess) {
      val stateView: EncryStateView = EncryStateView(height, lastBlockTimestamp)
      //race many
      val s1: Task[IndexedSeq[(EncryBaseBox, Input)]] = Task.sequence(tx.inputs.map {
        input =>
          val m: Task[Option[(EncryBaseBox, Input)]] = Task {
            logger.info(s"Execute task 1 for ${Algos.encode(input.boxId)}")
            storage.get(StorageKey !@@ input.boxId).flatMap {
              bytes => StateModifierSerializer.parseBytes(bytes, input.boxId.head).toOption
            }.map(_ -> input)
          }
          m
      }).map(_.flatten)

      val s2: Task[IndexedSeq[EncryBaseBox]] =
        s1.map(_.foldLeft(IndexedSeq.empty[EncryBaseBox]) { case (acc, (bxOpt, input)) =>
          println(s"Execute task 2 for input ${Algos.encode(input.boxId)}")
          (bxOpt, tx.defaultProofOpt) match {
            // If no `proofs` provided, then `defaultProof` is used.
            case (bx, defaultProofOpt) if input.proofs.nonEmpty =>
              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(tx, bx, stateView), input.contract,
                defaultProofOpt.map(input.proofs :+ _).getOrElse(input.proofs))) acc :+ bx else acc
            case (bx, Some(defaultProof)) =>
              if (EncryPropositionFunctions.canUnlock(bx.proposition,
                Context(tx, bx, stateView), input.contract, Seq(defaultProof))) acc :+ bx else acc
            case (bx, defaultProofOpt) =>
              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(tx, bx, stateView), input.contract,
                defaultProofOpt.map(Seq(_)).getOrElse(Seq.empty))) acc :+ bx else acc
            case _ => acc
          }
        })

      val s3: Task[Boolean] = s2.map { bxs =>
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

      val s4: Task[Either[ValidationResult, Transaction]] = s3.map { res =>
        if (!res) {
          logger.info(s"Tx: ${Algos.encode(tx.id)} invalid. Reason: Non-positive balance in $tx")
          Invalid(Seq(MalformedModifierError(s"Non-positive balance in $tx"))).asLeft[Transaction]
        } else tx.asRight[ValidationResult]
      }

      //      val bxs: IndexedSeq[EncryBaseBox] = tx.inputs.flatMap(input => storage.get(StorageKey !@@ input.boxId)
      //        .map(bytes => StateModifierSerializer.parseBytes(bytes, input.boxId.head))
      //        .map(_.toOption -> input))
      //        .foldLeft(IndexedSeq[EncryBaseBox]()) { case (acc, (bxOpt, input)) =>
      //          (bxOpt, tx.defaultProofOpt) match {
      //            // If no `proofs` provided, then `defaultProof` is used.
      //            case (Some(bx), defaultProofOpt) if input.proofs.nonEmpty =>
      //              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(tx, bx, stateView), input.contract,
      //                defaultProofOpt.map(input.proofs :+ _).getOrElse(input.proofs))) acc :+ bx else acc
      //            case (Some(bx), Some(defaultProof)) =>
      //              if (EncryPropositionFunctions.canUnlock(bx.proposition,
      //                Context(tx, bx, stateView), input.contract, Seq(defaultProof))) acc :+ bx else acc
      //            case (Some(bx), defaultProofOpt) =>
      //              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(tx, bx, stateView), input.contract,
      //                defaultProofOpt.map(Seq(_)).getOrElse(Seq.empty))) acc :+ bx else acc
      //            case _ => acc
      //          }
      //        }
      //
      //      val validBalance: Boolean = {
      //        val debitB: Map[String, Amount] = BalanceCalculator.balanceSheet(bxs).map {
      //          case (key, value) => Algos.encode(key) -> value
      //        }
      //        val creditB: Map[String, Amount] = {
      //          val balanceSheet: Map[TokenId, Amount] =
      //            BalanceCalculator.balanceSheet(tx.newBoxes, excludeTokenIssuance = true)
      //          val intrinsicBalance: Amount = balanceSheet.getOrElse(TestNetConstants.IntrinsicTokenId, 0L)
      //          balanceSheet.updated(TestNetConstants.IntrinsicTokenId, intrinsicBalance + tx.fee)
      //        }.map { case (tokenId, amount) => Algos.encode(tokenId) -> amount }
      //        creditB.forall { case (tokenId, amount) =>
      //          if (tokenId == Algos.encode(TestNetConstants.IntrinsicTokenId))
      //            debitB.getOrElse(tokenId, 0L) + allowedOutputDelta >= amount
      //          else debitB.getOrElse(tokenId, 0L) >= amount
      //        }
      //      }
      //
      //      if (!validBalance) {
      //        logger.info(s"Tx: ${Algos.encode(tx.id)} invalid. Reason: Non-positive balance in $tx")
      //        Invalid(Seq(MalformedModifierError(s"Non-positive balance in $tx"))).asLeft[Transaction]
      //      }
      //      else if (bxs.length != tx.inputs.length) {
      //        logger.info(s"Tx: ${Algos.encode(tx.id)} invalid. Reason: Box not found")
      //        Invalid(Seq(MalformedModifierError(s"Box not found"))).asLeft[Transaction]
      //      }
      //      else tx.asRight[ValidationResult]

      s4

    } else {
      Task.pure(tx.semanticValidity.errors.headOption
        .map(err => Invalid(Seq(err)).asLeft[Transaction])
        .getOrElse(tx.asRight[ValidationResult]))
    }

  def applyModifier(mod: PersistentModifier): Future[Either[List[ModifierApplyError], UtxoState]] = {
    val startTime = System.currentTimeMillis()
    val result = mod match {
      case header: Header =>
        logger.info(s"Starting to applyModifier as a header: ${Algos.encode(mod.id)} to state at height ${header.height}")
        Future.successful(UtxoState(
          storage,
          height,
          header.timestamp
        ).asRight[List[ModifierApplyError]])
      case block: Block =>
        logger.info(s"Starting to applyModifier as a block: ${Algos.encode(mod.id)} to state at height ${block.header.height}")
        val lastTxId = block.payload.txs.last.id
        val totalFees: Amount = block.payload.txs.init.map(_.fee).sum
        val validstartTime = System.currentTimeMillis()
        val res: Future[Either[ValidationResult, List[Transaction]]] = Future.sequence(block.payload.txs.map(tx => {

          if (tx.id sameElements lastTxId) validateTransaction(tx, totalFees + EncrySupplyController.supplyAt(height))
          else validateTransaction(tx)

        })).map(_.toList.traverse(Validated.fromEither).toEither)
//        val res: Either[ValidationResult, List[Transaction]] = block.payload.txs.map(tx => {
//
//          if (tx.id sameElements lastTxId) validateTransaction(tx, totalFees + EncrySupplyController.supplyAt(height))
//          else validateTransaction(tx)
//
//        }).toList
//          .traverse(Validated.fromEither)
//          .toEither
        logger.info(s"Validation time: ${(System.currentTimeMillis() - validstartTime) / 1000L} s")
        res.map(_.fold(
          err => err.errors.map(modError => StateModifierApplyError(modError.message)).toList.asLeft[UtxoState],
          txsToApply => {
            val combineTimeStart = System.currentTimeMillis()
            val combinedStateChange = combineAll(txsToApply.map(UtxoState.tx2StateChange))
            logger.info(s"Time of combining: ${(System.currentTimeMillis() - combineTimeStart) / 1000L} s")
            val insertTimestart = System.currentTimeMillis()
            storage.insert(
              StorageVersion !@@ block.id,
              combinedStateChange.outputsToDb.toList,
              combinedStateChange.inputsToDb.toList
            )
            logger.info(s"Time of insert: ${(System.currentTimeMillis() - insertTimestart) / 1000L} s")
            UtxoState(
              storage,
              Height @@ block.header.height,
              block.header.timestamp
            ).asRight[List[ModifierApplyError]]
          }
        ))
    }
    logger.info(s"Time of applying mod ${Algos.encode(mod.id)} of type ${mod.modifierTypeId} is (${(System.currentTimeMillis() - startTime) / 1000L} s)")
    result
  }

  def rollbackTo(version: VersionTag): Try[UtxoState] = Try {
    storage.versions.find(_ sameElements version) match {
      case Some(_) =>
        logger.info(s"Rollback to version ${Algos.encode(version)}")
        storage.rollbackTo(StorageVersion !@@ version)
        val stateHeight: Int = storage.get(StorageKey @@ UtxoState.bestHeightKey.untag(Digest32))
          .map(d => Ints.fromByteArray(d)).getOrElse(TestNetConstants.GenesisHeight)
        UtxoState(
          storage,
          Height @@ stateHeight,
          lastBlockTimestamp
        )
      case None => throw new Exception(s"Impossible to rollback to version ${Algos.encode(version)}")
    }
  }

  // In order to evaluate tasks, we'll need a Scheduler
  import monix.execution.Scheduler.Implicits.global

  // A Future type that is also Cancelable
  import monix.execution.CancelableFuture

  // Task is in monix.eval
  import monix.eval.Task
  import scala.util.{Success, Failure}
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._



    //    val cancelable = task.runAsync(
    //      new Callback[Throwable, Int] {
    //        def onSuccess(value: Int): Unit =
    //          println(value)
    //        def onError(ex: Throwable): Unit =
    //          System.err.println(s"ERROR: ${ex.getMessage}")
    //      })
    //    {
    //      case Right(value) =>
    //        println(value)
    //      case Left(ex) =>
    //        System.out.println(s"ERROR: ${ex}")
    //    }
    //
    //    Task {
    //      1 + 1
    //    }.runAsync { res =>
    //      res match {
    //        case Success(value) => println(value)
    //        case Failure(ex) => println(s"ERROR: ${ex.getMessage}")
    //      }
    //    }



//  def validate(tx: Transaction, allowedOutputDelta: Amount = 0L): Task[Either[ValidationResult, Transaction]] =
//    if (tx.semanticValidity.isSuccess) {
//      val stateView: EncryStateView = EncryStateView(height, lastBlockTimestamp, ADDigest @@ Array.emptyByteArray)
//
//      //race many
//
//      val s1: Task[IndexedSeq[(EncryBaseBox, Input)]] = Task.sequence(tx.inputs.map {
//        input =>
//          val m: Task[Option[(EncryBaseBox, Input)]] = Task {
//            logger.info(s"Execute task 1 for ${Algos.encode(input.boxId)}")
//            storage.get(StorageKey !@@ input.boxId).flatMap {
//              bytes => StateModifierSerializer.parseBytes(bytes, input.boxId.head).toOption
//            }.map(_ -> input)
//          }
//          m
//      }).map(_.flatten)
//
//      val s2: Task[IndexedSeq[EncryBaseBox]] =
//        s1.map(_.foldLeft(IndexedSeq.empty[EncryBaseBox]) { case (acc, (bxOpt, input)) =>
//          println(s"Execute task 2 for input ${Algos.encode(input.boxId)}")
//          (bxOpt, tx.defaultProofOpt) match {
//            // If no `proofs` provided, then `defaultProof` is used.
//            case (bx, defaultProofOpt) if input.proofs.nonEmpty =>
//              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(tx, bx, stateView), input.contract,
//                defaultProofOpt.map(input.proofs :+ _).getOrElse(input.proofs))) acc :+ bx else acc
//            case (bx, Some(defaultProof)) =>
//              if (EncryPropositionFunctions.canUnlock(bx.proposition,
//                Context(tx, bx, stateView), input.contract, Seq(defaultProof))) acc :+ bx else acc
//            case (bx, defaultProofOpt) =>
//              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(tx, bx, stateView), input.contract,
//                defaultProofOpt.map(Seq(_)).getOrElse(Seq.empty))) acc :+ bx else acc
//            case _ => acc
//          }
//        })
//
//      val s3: Task[Boolean] = s2.map { bxs =>
//        val debitB: Map[String, Amount] = BalanceCalculator.balanceSheet(bxs).map {
//          case (key, value) => Algos.encode(key) -> value
//        }
//        val creditB: Map[String, Amount] = {
//          val balanceSheet: Map[TokenId, Amount] =
//            BalanceCalculator.balanceSheet(tx.newBoxes, excludeTokenIssuance = true)
//          val intrinsicBalance: Amount = balanceSheet.getOrElse(TestNetConstants.IntrinsicTokenId, 0L)
//          balanceSheet.updated(TestNetConstants.IntrinsicTokenId, intrinsicBalance + tx.fee)
//        }.map { case (tokenId, amount) => Algos.encode(tokenId) -> amount }
//        creditB.forall { case (tokenId, amount) =>
//          if (tokenId == Algos.encode(TestNetConstants.IntrinsicTokenId))
//            debitB.getOrElse(tokenId, 0L) + allowedOutputDelta >= amount
//          else debitB.getOrElse(tokenId, 0L) >= amount
//        }
//      }
//
//      val s4: Task[Either[ValidationResult, Transaction]] = s3.map { res =>
//        if (!res) {
//          logger.info(s"Tx: ${Algos.encode(tx.id)} invalid. Reason: Non-positive balance in $tx")
//          Invalid(Seq(MalformedModifierError(s"Non-positive balance in $tx"))).asLeft[Transaction]
//        } else tx.asRight[ValidationResult]
//      }
//
//      //      val bxs: IndexedSeq[EncryBaseBox] = tx.inputs.flatMap(input => storage.get(StorageKey !@@ input.boxId)
//      //        .map(bytes => StateModifierSerializer.parseBytes(bytes, input.boxId.head))
//      //        .map(_.toOption -> input))
//      //        .foldLeft(IndexedSeq[EncryBaseBox]()) { case (acc, (bxOpt, input)) =>
//      //          (bxOpt, tx.defaultProofOpt) match {
//      //            // If no `proofs` provided, then `defaultProof` is used.
//      //            case (Some(bx), defaultProofOpt) if input.proofs.nonEmpty =>
//      //              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(tx, bx, stateView), input.contract,
//      //                defaultProofOpt.map(input.proofs :+ _).getOrElse(input.proofs))) acc :+ bx else acc
//      //            case (Some(bx), Some(defaultProof)) =>
//      //              if (EncryPropositionFunctions.canUnlock(bx.proposition,
//      //                Context(tx, bx, stateView), input.contract, Seq(defaultProof))) acc :+ bx else acc
//      //            case (Some(bx), defaultProofOpt) =>
//      //              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(tx, bx, stateView), input.contract,
//      //                defaultProofOpt.map(Seq(_)).getOrElse(Seq.empty))) acc :+ bx else acc
//      //            case _ => acc
//      //          }
//      //        }
//      //
//      //      val validBalance: Boolean = {
//      //        val debitB: Map[String, Amount] = BalanceCalculator.balanceSheet(bxs).map {
//      //          case (key, value) => Algos.encode(key) -> value
//      //        }
//      //        val creditB: Map[String, Amount] = {
//      //          val balanceSheet: Map[TokenId, Amount] =
//      //            BalanceCalculator.balanceSheet(tx.newBoxes, excludeTokenIssuance = true)
//      //          val intrinsicBalance: Amount = balanceSheet.getOrElse(TestNetConstants.IntrinsicTokenId, 0L)
//      //          balanceSheet.updated(TestNetConstants.IntrinsicTokenId, intrinsicBalance + tx.fee)
//      //        }.map { case (tokenId, amount) => Algos.encode(tokenId) -> amount }
//      //        creditB.forall { case (tokenId, amount) =>
//      //          if (tokenId == Algos.encode(TestNetConstants.IntrinsicTokenId))
//      //            debitB.getOrElse(tokenId, 0L) + allowedOutputDelta >= amount
//      //          else debitB.getOrElse(tokenId, 0L) >= amount
//      //        }
//      //      }
//      //
//      //      if (!validBalance) {
//      //        logger.info(s"Tx: ${Algos.encode(tx.id)} invalid. Reason: Non-positive balance in $tx")
//      //        Invalid(Seq(MalformedModifierError(s"Non-positive balance in $tx"))).asLeft[Transaction]
//      //      }
//      //      else if (bxs.length != tx.inputs.length) {
//      //        logger.info(s"Tx: ${Algos.encode(tx.id)} invalid. Reason: Box not found")
//      //        Invalid(Seq(MalformedModifierError(s"Box not found"))).asLeft[Transaction]
//      //      }
//      //      else tx.asRight[ValidationResult]
//
//      s4
//
//    } else {
//      Task.pure(tx.semanticValidity.errors.headOption
//        .map(err => Invalid(Seq(err)).asLeft[Transaction])
//        .getOrElse(tx.asRight[ValidationResult]))
//    }

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
        IODBWrapper(new LSMStore(stateDir, keepVersions = TestNetConstants.DefaultKeepVersions))
      case VersionalStorage.LevelDB =>
        logger.info("Init state with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(stateDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(300, 32), keySize = 32))
    }
    val stateHeight: Int = versionalStorage.get(StorageKey @@ bestHeightKey.untag(Digest32))
      .map(d => Ints.fromByteArray(d)).getOrElse(TestNetConstants.PreGenesisHeight)
    val lastBlockTimestamp: Amount = versionalStorage.get(StorageKey @@ lastBlockTimeKey.untag(Digest32))
      .map(d => Longs.fromByteArray(d)).getOrElse(0L)
    new UtxoState(
      versionalStorage,
      Height @@ stateHeight,
      lastBlockTimestamp,
    )
  }

  def genesis(stateDir: File,
              nodeViewHolderRef: Option[ActorRef],
              settings: EncryAppSettings,
              statsSenderRef: Option[ActorRef]): UtxoState = {
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
      initialStateBoxes.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))
    )

    new UtxoState(
      storage,
      TestNetConstants.PreGenesisHeight,
      0L,
    )
  }
}