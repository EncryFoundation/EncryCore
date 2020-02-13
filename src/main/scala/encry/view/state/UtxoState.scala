package encry.view.state

import java.io.File

import akka.actor.ActorRef
import cats.data.Validated
import cats.instances.list._
import cats.syntax.either._
import cats.syntax.traverse._
import com.google.common.primitives.Ints
import com.typesafe.scalalogging.StrictLogging
import encry.consensus.EncrySupplyController
import encry.modifiers.state.{Context, EncryPropositionFunctions}
import encry.settings.EncryAppSettings
import encry.stats.StatsSender.UtxoStat
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.storage.{RootNodesStorage, VersionalStorage}
import encry.utils.BalanceCalculator
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.implicits.UTXO._
import encry.utils.implicits.Validation._
import encry.view.NodeViewErrors.ModifierApplyError
import encry.view.NodeViewErrors.ModifierApplyError.StateModifierApplyError
import encry.view.state.UtxoState.StateChange
import encry.view.state.avlTree.AvlTree
import encry.view.state.avlTree.utils.implicits.Instances._
import io.iohk.iodb.LSMStore
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

import scala.util.Try

final case class UtxoState(tree: AvlTree[StorageKey, StorageValue],
                           height: Height,
                           constants: Constants,
                           influxRef: Option[ActorRef]) extends StrictLogging with UtxoStateReader with AutoCloseable {

  def safePointHeight = tree.rootNodesStorage.safePointHeight

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
      influxRef
    )
  }

  def applyModifier(mod: PersistentModifier, saveRootNodes: Boolean = false): Either[List[ModifierApplyError], UtxoState] = {
    val startTime = System.currentTimeMillis()
    val result = mod match {
      case header: Header =>
        logger.info(s"\n\nStarting to applyModifier as a header: ${Algos.encode(mod.id)} to state at height ${header.height}")
        UtxoState(tree, height, constants, influxRef).asRight[List[ModifierApplyError]]
      case block: Block =>
        logger.info(s"\n\nStarting to applyModifier as a Block: ${Algos.encode(mod.id)} to state at height ${block.header.height}")
        logger.info(s"State root should be: ${Algos.encode(block.header.stateRoot)}")
        logger.info(s"Current root node hash: ${tree.rootNode.hash}")
        val lastTxId = block.payload.txs.last.id
        val totalFees: Amount = block.payload.txs.init.map(_.fee).sum
        val validstartTime = System.nanoTime()
        val res: Either[ValidationResult, List[Transaction]] = block.payload.txs.map(tx => {
          if (tx.id sameElements lastTxId) validate(tx, block.header.timestamp, Height @@ block.header.height,
            totalFees + EncrySupplyController.supplyAt(Height @@ block.header.height, constants))
          else validate(tx, block.header.timestamp, Height @@ block.header.height)
        }).toList
          .traverse(Validated.fromEither)
          .toEither
        val validationTime = System.nanoTime() - validstartTime
        //todo: influx ref doesn't init during restart
        influxRef.foreach(_ ! UtxoStat(
          block.payload.txs.length,
          validationTime
        ))
        logger.info(s"Validation time: ${validationTime/1000000} ms. Txs: ${block.payload.txs.length}")
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
            val newTree: AvlTree[StorageKey, StorageValue] = tree.insertAndDeleteMany(
              StorageVersion !@@ block.id,
              combinedStateChange.outputsToDb.toList,
              combinedStateChange.inputsToDb.toList,
              Height @@ block.header.height,
              saveRootNodes
            )
            logger.info(s"Time of insert: ${(System.currentTimeMillis() - insertTimestart) / 1000L} s")
            logger.info(s"newTree.rootNode.hash ${Algos.encode(newTree.rootNode.hash)}.")
            logger.info(s"block.header.stateRoot ${Algos.encode(block.header.stateRoot)}")
            if (!(newTree.rootNode.hash sameElements block.header.stateRoot)) {
              logger.info(s"Invalid state root!")
              List(StateModifierApplyError(s"Incorrect state root after block (${block.header.encodedId}) applying. " +
                s"State root should be ${Algos.encode(block.header.stateRoot)} but got " +
                s"${Algos.encode(newTree.rootNode.hash)}")).asLeft[UtxoState]
            } else {
              logger.info(s"After applying root node: ${newTree.rootNode.hash}")
              UtxoState(
                newTree,
                Height @@ block.header.height,
                constants,
                influxRef,
              ).asRight[List[ModifierApplyError]]
            }
          }
        )
    }
    logger.info(s"Time of applying mod ${Algos.encode(mod.id)} of type ${mod.modifierTypeId} is (${(System.currentTimeMillis() - startTime) / 1000L} s)")
    result
  }


  def rollbackTo(version: VersionTag, additionalBlocks: List[Block]): Try[UtxoState] = Try{
    logger.info(s"Rollback utxo to version: ${Algos.encode(version)}")
    val rollbackedAvl = tree.rollbackTo(StorageVersion !@@ version, additionalBlocks).get
    logger.info(s"UTXO -> rollbackTo ->${tree.avlStorage.get(UtxoState.bestHeightKey)} ")
    val height: Height = Height !@@ Ints.fromByteArray(tree.avlStorage.get(UtxoState.bestHeightKey).get)
    UtxoState(rollbackedAvl, height, constants, influxRef)
  }

  def restore(additionalBlocks: List[Block]): Try[UtxoState] = Try {
    logger.info(s"Rollback utxo from storage: ${Algos.encode(version)}")
    val rollbackedAvl = tree.restore(additionalBlocks).get
    logger.info(s"UTXO -> rollbackTo ->${tree.avlStorage.get(UtxoState.bestHeightKey)} ")
    val height: Height = Height !@@ Ints.fromByteArray(tree.avlStorage.get(UtxoState.bestHeightKey).get)
    UtxoState(rollbackedAvl, height, constants, influxRef)
  }

  def validate(tx: Transaction, blockTimeStamp: Long, blockHeight: Height, allowedOutputDelta: Amount = 0L): Either[ValidationResult, Transaction] =
    if (tx.semanticValidity.isSuccess) {
      val stateView: EncryStateView = EncryStateView(blockHeight, blockTimeStamp, ADDigest @@ Array.emptyByteArray)
      val bxs: IndexedSeq[EncryBaseBox] = tx.inputs.flatMap(input => {
        val res = tree.get(StorageKey !@@ input.boxId)
        res.flatMap(bytes => {
          val bx = StateModifierSerializer.parseBytes(bytes, input.boxId.head)
          val bxOpt = bx.toOption
          (bxOpt, tx.defaultProofOpt) match {
            // If no `proofs` provided, then `defaultProof` is used.
            case (Some(bx), defaultProofOpt) if input.proofs.nonEmpty =>
              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(tx, bx, stateView), input.contract,
                defaultProofOpt.map(input.proofs :+ _).getOrElse(input.proofs))) Some(bx) else None
            case (Some(bx), Some(defaultProof)) =>
              if (EncryPropositionFunctions.canUnlock(bx.proposition,
                Context(tx, bx, stateView), input.contract, Seq(defaultProof))) Some(bx) else None
            case (Some(bx), defaultProofOpt) =>
              if (EncryPropositionFunctions.canUnlock(bx.proposition, Context(tx, bx, stateView), input.contract,
                defaultProofOpt.map(Seq(_)).getOrElse(Seq.empty))) Some(bx) else None
            case _ => None
          }
        })
      })

      val validBalance: Boolean = {
        val debitB: Map[String, Amount] = BalanceCalculator.balanceSheet(bxs, constants.IntrinsicTokenId).map {
          case ((_, key), value) => Algos.encode(key) -> value
        }
        val creditB: Map[String, Amount] = {
          val balanceSheet: Map[TokenId, Amount] =
            BalanceCalculator.balanceSheet(tx.newBoxes, constants.IntrinsicTokenId, excludeTokenIssuance = true).map {
              case ((_, key), value) => key -> value
            }
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

  val bestHeightKey: StorageKey = StorageKey !@@ ((3: Byte) +: Algos.hash("state_height"))

  def tx2StateChange(tx: Transaction): StateChange = StateChange(
    tx.inputs.map(input => StorageKey !@@ input.boxId).toVector,
    tx.newBoxes.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes)).toVector
  )

  def initialStateBoxes: List[AssetBox] = List(AssetBox(EncryProposition.open, -9, 0))

  def getStateDir(settings: EncryAppSettings): File = {
    logger.info(s"Invoke getStateDir")
    if (settings.snapshotSettings.enableFastSynchronization) {
      logger.info(s"Start state with tmp folder")
      new File(s"${settings.directory}/tmpDirState")
    }
    else {
      logger.info(s"Start state with main folder")
      new File(s"${settings.directory}/state")
    }
  }

  def getRootsDir(settings: EncryAppSettings): File = {
    logger.info(s"Invoke getRootsDir")
    new File(s"${settings.directory}/roots")
  }

  def create(stateDir: File, rootStorageDir: File, settings: EncryAppSettings, influxRef: Option[ActorRef]): UtxoState = {
    val versionalStorage = settings.storage.state match {
      case VersionalStorage.IODB =>
        logger.info("Init state with iodb storage")
        IODBWrapper(new LSMStore(stateDir, keepVersions = settings.constants.DefaultKeepVersions))
      case VersionalStorage.LevelDB =>
        logger.info("Init state with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(stateDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB.copy(keySize = 33), keySize = 33))
    }
    val rootStorage = {
      val levelDBInit = LevelDbFactory.factory.open(rootStorageDir, new Options)
      RootNodesStorage[StorageKey, StorageValue](levelDBInit, settings.constants.MaxRollbackDepth, rootStorageDir)
    }
    val height = Height @@ Ints.fromByteArray(versionalStorage.get(UtxoState.bestHeightKey).get)
    logger.info(s"State created.")
    UtxoState(
      AvlTree[StorageKey, StorageValue](versionalStorage, rootStorage),
      height,
      settings.constants,
      influxRef
    )
  }

  def genesis(stateDir: File, rootStorageDir: File, settings: EncryAppSettings, influxRef: Option[ActorRef]): UtxoState = {
    //check kind of storage
    val storage = settings.storage.state match {
      case VersionalStorage.IODB =>
        logger.info("Init state with iodb storage")
        IODBWrapper(new LSMStore(stateDir, keepVersions = settings.constants.DefaultKeepVersions, keySize = 33))
      case VersionalStorage.LevelDB =>
        logger.info("Init state with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(stateDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB.copy(keySize = 33), keySize = 33))
    }
    val rootStorage = {
      val levelDBInit = LevelDbFactory.factory.open(rootStorageDir, new Options)
      RootNodesStorage[StorageKey, StorageValue](levelDBInit, settings.constants.MaxRollbackDepth, rootStorageDir)
    }
    storage.insert(
      StorageVersion @@ Array.fill(32)(0: Byte),
      initialStateBoxes.map(bx => (StorageKey !@@ AvlTree.elementKey(bx.id), StorageValue @@ bx.bytes))
    )
    UtxoState(AvlTree[StorageKey, StorageValue](storage, rootStorage), Height @@ 0, settings.constants, influxRef)
  }
}
