package encry.view.state

import java.io.File

import NodeMsg.NodeProtoMsg
import cats.data.Validated
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import com.google.common.primitives.Ints
import encry.utils.implicits.Validation._
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
import encry.view.NodeViewErrors.ModifierApplyError
import encry.view.NodeViewErrors.ModifierApplyError.StateModifierApplyError
import encry.view.fast.sync.SnapshotHolder.SnapshotChunk
import encry.view.state.UtxoState.StateChange
import encry.view.state.avlTree.{AvlTree, Node, NodeSerilalizer}
import encry.view.state.avlTree.utils.implicits.Instances._
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
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
import scorex.crypto.hash.Digest32

import scala.collection.immutable
import scala.util.Try

final case class UtxoState(tree: AvlTree[StorageKey, StorageValue],
                           height: Height,
                           constants: Constants) extends StrictLogging with UtxoStateReader with AutoCloseable {

  def applyValidModifier(block: Block): UtxoState = {
    logger.info(s"Block validated successfully. Inserting changes to storage.")
    val combinedStateChange: StateChange = combineAll(block.payload.txs.toList.map(UtxoState.tx2StateChange))
    val newTree = tree.insertAndDeleteMany(
      StorageVersion !@@ block.id,
      combinedStateChange.outputsToDb.toList,
      combinedStateChange.inputsToDb.toList,
      Height @@ block.header.height
    )
    UtxoState(
      newTree,
      Height @@ block.header.height,
      constants,
    )
  }

  def applyModifier(mod: PersistentModifier): Either[List[ModifierApplyError], UtxoState] = {
    val startTime = System.currentTimeMillis()
    val result = mod match {
      case header: Header =>
        logger.info(s"\n\nStarting to applyModifier as a header: ${Algos.encode(mod.id)} to state at height ${header.height}")
        UtxoState(tree, height, constants).asRight[List[ModifierApplyError]]
      case block: Block =>
        logger.info(s"\n\nStarting to applyModifier as a Block: ${Algos.encode(mod.id)} to state at height ${block.header.height}")
        logger.info(s"State root should be: ${Algos.encode(block.header.stateRoot)}")
        logger.info(s"Current root node: ${tree.rootNode}")
        val lastTxId = block.payload.txs.last.id
        val totalFees: Amount = block.payload.txs.init.map(_.fee).sum
        val validstartTime = System.currentTimeMillis()
        val res: Either[ValidationResult, List[Transaction]] = block.payload.txs.map(tx => {
          if (tx.id sameElements lastTxId) validate(tx, block.header.timestamp, Height @@ block.header.height,
            totalFees + EncrySupplyController.supplyAt(Height @@ block.header.height, constants))
          else validate(tx, block.header.timestamp, Height @@ block.header.height)
        }).toList
          .traverse(Validated.fromEither)
          .toEither
        logger.info(s"Validation time: ${(System.currentTimeMillis() - validstartTime) / 1000L} s")
        res.fold(
          err => {
            logger.info(s"Failed to state cause ${err.message}")
            err.errors.map(modError => StateModifierApplyError(modError.message)).toList.asLeft[UtxoState]
          },
          txsToApply => {
            val combineTimeStart = System.currentTimeMillis()
            val combinedStateChange: UtxoState.StateChange = combineAll(txsToApply.map(UtxoState.tx2StateChange))
            logger.info(s"Time of combining: ${(System.currentTimeMillis() - combineTimeStart) / 1000L} s")
            val insertTimestart = System.currentTimeMillis()
            logger.info(s"applyModifier -> newTree ${tree.storage.getAllKeys(-1).map(Algos.encode(_))}")
            logger.info(s"applyModifier -> outputsToDb -> ${combinedStateChange.outputsToDb.map(j => Algos.encode(j._1))}")
            logger.info(s"applyModifier -> inputsToDb -> ${combinedStateChange.inputsToDb.map(j => Algos.encode(j))}")
            logger.info(s"jfkdsvkjdfh ${
              tree.storage
                .get(StorageKey @@ Algos.decode("ffdb114b3295e7d8cc4fed1b83de62a4bb0c4806cb4abcd04ac6b545162b4f6c").get
              )}")
            val newTree: AvlTree[StorageKey, StorageValue] = tree.insertAndDeleteMany(
              StorageVersion !@@ block.id,
              combinedStateChange.outputsToDb.toList,
              combinedStateChange.inputsToDb.toList,
              Height @@ block.header.height
            )
            logger.info(s"newTree.rootNode.hash ${Algos.encode(newTree.rootNode.hash)}")
            logger.info(s"block.header.stateRoot ${Algos.encode(block.header.stateRoot)}")
            if (!(newTree.rootNode.hash sameElements block.header.stateRoot)) {
              logger.info(s"Invalid state root!")
              List(StateModifierApplyError(s"Incorrect state root after block (${block.header.encodedId}) applying. " +
                s"State root should be ${Algos.encode(block.header.stateRoot)} but got " +
                s"${Algos.encode(newTree.rootNode.hash)}")).asLeft[UtxoState]
            } else {
              logger.info(s"Time of insert: ${(System.currentTimeMillis() - insertTimestart) / 1000L} s")
              logger.info(s"After applying root node: ${newTree.rootNode}")
              UtxoState(
                newTree,
                Height @@ block.header.height,
                constants,
              ).asRight[List[ModifierApplyError]]
            }
          }
        )
    }
    logger.info(s"Time of applying mod ${Algos.encode(mod.id)} of type ${mod.modifierTypeId} is (${(System.currentTimeMillis() - startTime) / 1000L} s)")
    result
  }

  def rollbackTo(version: VersionTag): Try[UtxoState] = Try{
    val rollbackedAvl = tree.rollbackTo(StorageVersion !@@ version).get
    logger.info(s"UTXO -> rollbackTo ->${tree.storage.get(UtxoState.bestHeightKey)} ")
    val height: Height = Height !@@ Ints.fromByteArray(tree.storage.get(UtxoState.bestHeightKey).get)
    UtxoState(rollbackedAvl, height, constants)
  }

  def validate(tx: Transaction, blockTimeStamp: Long, blockHeight: Height, allowedOutputDelta: Amount = 0L): Either[ValidationResult, Transaction] =
    if (tx.semanticValidity.isSuccess) {
      val stateView: EncryStateView = EncryStateView(blockHeight, blockTimeStamp, ADDigest @@ Array.emptyByteArray)
      val bxs: IndexedSeq[EncryBaseBox] = tx.inputs.flatMap(input => tree.get(StorageKey !@@ input.boxId)
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

  def close(): Unit = tree.close()
}

object UtxoState extends StrictLogging {

  final case class StateChange(inputsToDb: Vector[StorageKey],
                               outputsToDb: Vector[(StorageKey, StorageValue)])

  val bestHeightKey: StorageKey = StorageKey !@@ Algos.hash("state_height")

  def tx2StateChange(tx: Transaction): StateChange = StateChange(
    tx.inputs.map(input => StorageKey !@@ input.boxId).toVector,
    tx.newBoxes.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes)).toVector
  )

  def initialStateBoxes: List[AssetBox] = List(AssetBox(EncryProposition.open, -9, 0))

  def getStateDir(settings: EncryAppSettings): File = new File(s"${settings.directory}/state")

  def create(stateDir: File, settings: EncryAppSettings): UtxoState = {
    val versionalStorage = settings.storage.state match {
      case VersionalStorage.IODB =>
        logger.info("Init state with iodb storage")
        IODBWrapper(new LSMStore(stateDir, keepVersions = settings.constants.DefaultKeepVersions))
      case VersionalStorage.LevelDB =>
        logger.info("Init state with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(stateDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(300, 32), keySize = 32))
    }
    logger.info(s"State created.")
    UtxoState(
      AvlTree[StorageKey, StorageValue](versionalStorage),
      Height @@ 0,
      settings.constants
    )
  }

  def genesis(stateDir: File, settings: EncryAppSettings): UtxoState = {
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
    if (!settings.snapshotSettings.enableFastSynchronization) storage.insert(
      StorageVersion @@ Array.fill(32)(0: Byte),
      initialStateBoxes.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))
    )
    UtxoState(AvlTree[StorageKey, StorageValue](storage), Height @@ 0, settings.constants)
  }
}
