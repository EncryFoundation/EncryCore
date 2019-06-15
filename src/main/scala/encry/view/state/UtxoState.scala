package encry.view.state

import java.io.File

import akka.actor.ActorRef
import com.google.common.primitives.{Ints, Longs}
import com.typesafe.scalalogging.StrictLogging
import encry.avltree.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedAVLStorage}
import encry.consensus.EncrySupplyController
import encry.modifiers.history.ADProofsUtils
import encry.modifiers.state.{Context, EncryPropositionFunctions}
import encry.settings.{EncryAppSettings, LevelDBSettings, TestConstants}
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.BalanceCalculator
import encry.stats.StatsSender.TxsInBlock
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.StorageKey
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.VersionalLevelDbValue
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.view.EncryNodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ADProofs, Block, Header}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.modifiers.state.StateModifierSerializer
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.{EncryBaseBox, EncryBox}
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.Algos.HF
import org.encryfoundation.common.utils.TaggedTypes.{Height, _}
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.ValidationResult.{Invalid, Valid}
import org.encryfoundation.common.validation.{MalformedModifierError, ValidationResult}
import org.iq80.leveldb.Options
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Success, Try}

class UtxoState(override val persistentProver: encry.avltree.PersistentBatchAVLProver[Digest32, HF],
                override val version: VersionTag,
                override val height: Height,
                override val stateStore: VersionalStorage,
                val lastBlockTimestamp: Long,
                nodeViewHolderRef: Option[ActorRef],
                override val settings: EncryAppSettings,
                statsSenderRef: Option[ActorRef])
  extends EncryState[UtxoState] with UtxoStateReader with StrictLogging {

  import UtxoState.metadata

  override val rootHash: ADDigest = persistentProver.storage.store
    .get(StorageKey !@@ Algos.hash(version)).map(value => ADDigest !@@ value)
    .getOrElse(persistentProver.digest)

  def maxRollbackDepth: Int = TestNetConstants.MaxRollbackDepth

  private def onAdProofGenerated(proof: ADProofs): Unit = {
    if (nodeViewHolderRef.isEmpty) logger.warn(s"Got proof while nodeViewHolderRef is empty")
    nodeViewHolderRef.foreach(_ ! LocallyGeneratedModifier(proof))
  }

  def applyBlockTransactions(blockTransactions: Seq[Transaction],
                             expectedDigest: ADDigest): Try[Unit] = {
    def applyTry(txs: Seq[Transaction], allowedOutputDelta: Amount = 0L): Try[Unit] =
      txs.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, tx) =>
        t.flatMap { _ =>
          if (validate(tx, allowedOutputDelta).isSuccess) {
            extractStateChanges(tx).operations.map(ADProofsUtils.toModification)
              .foldLeft[Try[Option[ADValue]]](Success(None)) { case (tIn, m) =>
              tIn.flatMap(_ => persistentProver.performOneOperation(m))
            }
          } else util.Failure(validate(tx, allowedOutputDelta).errors.head.toThrowable)
        }
      }.map(_ => Unit)

    val coinbase: Transaction = blockTransactions.last
    val regularTransactions: Seq[Transaction] = blockTransactions.init

    val totalFees: Amount = regularTransactions.map(_.fee).sum

    val regularApplyTry: Try[Unit] = applyTry(regularTransactions)
    val coinbaseApplyTry: Try[Unit] = applyTry(Seq(coinbase), totalFees + EncrySupplyController.supplyAt(height))

    regularApplyTry.flatMap(_ => coinbaseApplyTry).map { _ =>
      if (!expectedDigest.sameElements(persistentProver.digest))
        throw new Exception(s"Digest after txs application is wrong. ${Algos.encode(expectedDigest)} expected, " +
          s"${Algos.encode(persistentProver.digest)} given")
    }
  }

  override def applyModifier(mod: PersistentModifier): Try[UtxoState] = mod match {
    //here
    case block: Block =>
      logger.info(s"\n\nStarting to applyModifier as a Block: ${Algos.encode(mod.id)}!")
      val startTime = System.currentTimeMillis()
      logger.debug(s"Applying block with header ${block.header.encodedId} to UtxoState with " +
        s"root hash ${Algos.encode(rootHash)} at height $height.")
      statsSenderRef.foreach(_ ! TxsInBlock(block.payload.txs.size))
      applyBlockTransactions(block.payload.txs, block.header.stateRoot).map { _ =>
        val meta: Seq[(Array[Byte], Array[Byte])] = metadata(
          VersionTag !@@ block.id,
          block.header.stateRoot,
          Height @@ block.header.height,
          block.header.timestamp
        )
        val proofBytes: SerializedAdProof = persistentProver.generateProofAndUpdateStorage(meta)
        logger.debug(s"starting generating proofHash!")
        val timer1 = System.currentTimeMillis()
        val proofHash: Digest32 = ADProofs.proofDigest(proofBytes)
        logger.debug(s"Finifhsing generating proofHash! Process time is: ${System.currentTimeMillis() - timer1}")

        if (block.adProofsOpt.isEmpty && settings.node.stateMode.isDigest)
          onAdProofGenerated(ADProofs(block.header.id, proofBytes))
        logger.info(s"Valid modifier ${block.encodedId} with header ${block.header.encodedId} applied to UtxoState with" +
          s" root hash ${Algos.encode(rootHash)}")

        if (!stateStore.get(StorageKey @@ block.id.untag(ModifierId)).exists(_ sameElements block.header.stateRoot))
          throw new Exception("Storage kept roothash is not equal to the declared one.")
        else if (!stateStore.versions.exists(_ sameElements block.header.stateRoot))
          throw new Exception("Unable to apply modification properly.")
        else if (!(block.header.adProofsRoot sameElements proofHash))
          throw new Exception("Calculated proofHash is not equal to the declared one.")
        else if (!(block.header.stateRoot sameElements persistentProver.digest))
          throw new Exception("Calculated stateRoot is not equal to the declared one.")

        val a = new UtxoState(
          persistentProver,
          VersionTag !@@ block.id,
          Height @@ block.header.height,
          stateStore,
          lastBlockTimestamp,
          nodeViewHolderRef,
          settings,
          statsSenderRef
        )
        logger.debug(s"Finishing o applyModifier as a Block: ${Algos.encode(mod.id)}! Time is: ${System.currentTimeMillis() - startTime}")
        a
      }.recoverWith[UtxoState] { case e =>
        logger.info(s"Failed to apply block with header ${block.header.encodedId} to UTXOState with root" +
          s" ${Algos.encode(rootHash)}: $e")
        Failure(e)
      }

    case header: Header =>
      logger.debug(s"\n\nStarting to applyModifier as a Header: ${Algos.encode(mod.id)}!")
      val startTime = System.currentTimeMillis()

      val a = Success(new UtxoState(
        persistentProver,
        VersionTag !@@ header.id,
        height,
        stateStore,
        lastBlockTimestamp,
        nodeViewHolderRef,
        settings,
        statsSenderRef
      ))
      logger.debug(s"Finishing o applyModifier as a Header: ${Algos.encode(mod.id)}! Time is: ${System.currentTimeMillis() - startTime}")

      a

    case _ => Failure(new Exception("Got Modifier of unknown type."))
  }

  def generateProofs(txs: Seq[Transaction]): Try[(SerializedAdProof, ADDigest)] = Try {
    logger.info(s"Generating proof for ${txs.length} transactions ...")
    val rootHash: ADDigest = persistentProver.digest
    if (txs.isEmpty) throw new Exception("Got empty transaction sequence")
    else if (!storage.version.exists(_.sameElements(rootHash)))
      throw new Exception(s"Invalid storage version: ${storage.version.map(Algos.encode)} != ${Algos.encode(rootHash)}")
    persistentProver.avlProver.generateProofForOperations(extractStateChanges(txs).operations.map(ADProofsUtils.toModification))
  }.flatten.recoverWith[(SerializedAdProof, ADDigest)] { case e =>
    logger.info(s"Failed to generate ADProof cause $e")
    Failure(e)
  }

  override def rollbackTo(version: VersionTag): Try[UtxoState] = {
    logger.info(s"Rollback UtxoState to version ${Algos.encoder.encode(version)}")
    stateStore.get(StorageKey !@@ version) match {
      case Some(v) =>
        val rollbackResult: Try[UtxoState] = persistentProver.rollback(ADDigest !@@ v).map { _ =>
          val stateHeight: Int = stateStore.get(StorageKey @@ UtxoState.bestHeightKey.untag(Digest32))
            .map(d => Ints.fromByteArray(d)).getOrElse(TestNetConstants.GenesisHeight)
          new UtxoState(
            persistentProver,
            version,
            Height @@ stateHeight,
            stateStore,
            lastBlockTimestamp,
            nodeViewHolderRef,
            settings,
            statsSenderRef
          )
        }
        rollbackResult
      case None =>
        Failure(new Error(s"Unable to get root hash at version ${Algos.encoder.encode(version)}"))
    }
  }

  override def rollbackVersions: Iterable[VersionTag] =
    persistentProver.storage.rollbackVersions.map(v =>
      VersionTag @@ stateStore.get(StorageKey @@ Algos.hash(v).untag(Digest32)).get.untag(VersionalLevelDbValue))

  def validate(tx: Transaction, allowedOutputDelta: Amount = 0L): ValidationResult =
    if (tx.semanticValidity.isSuccess) {
      val stateView: EncryStateView = EncryStateView(height, lastBlockTimestamp, rootHash)
      val bxs: IndexedSeq[EncryBaseBox] = tx.inputs.flatMap(input => persistentProver.unauthenticatedLookup(input.boxId)
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
        Invalid(Seq(MalformedModifierError(s"Non-positive balance in $tx")))
      }
      else if (bxs.length != tx.inputs.length) {
        logger.info(s"Tx: ${Algos.encode(tx.id)} invalid. Reason: Box not found")
        Invalid(Seq(MalformedModifierError(s"Box not found")))
      }
      else Valid
    } else tx.semanticValidity

  def isValid(tx: Transaction, allowedOutputDelta: Amount = 0L): Boolean = validate(tx, allowedOutputDelta).isSuccess

  def filterValid(txs: Seq[Transaction]): Seq[Transaction] = txs.filter(tx => isValid(tx))
}

object UtxoState extends StrictLogging {

  private val bestVersionKey: Digest32 = Algos.hash("best_state_version")

  private val bestHeightKey: Digest32 = Algos.hash("state_height")

  private val lastBlockTimeKey: Digest32 = Algos.hash("last_block_timestamp")

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
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(300, 33), keySize = 33))
    }
    val stateVersion: Array[Byte] = versionalStorage.get(StorageKey @@ bestVersionKey.untag(Digest32))
      .map(_.untag(VersionalLevelDbValue)).getOrElse(EncryState.genesisStateVersion)
    val stateHeight: Int = versionalStorage.get(StorageKey @@ bestHeightKey.untag(Digest32))
      .map(d => Ints.fromByteArray(d)).getOrElse(TestNetConstants.PreGenesisHeight)
    val lastBlockTimestamp: Amount = versionalStorage.get(StorageKey @@ lastBlockTimeKey.untag(Digest32))
      .map(d => Longs.fromByteArray(d)).getOrElse(0L)
    val persistentProver: encry.avltree.PersistentBatchAVLProver[Digest32, HF] = {
      val bp: encry.avltree.BatchAVLProver[Digest32, HF] =
        new encry.avltree.BatchAVLProver[Digest32, Algos.HF](keyLength = 32, valueLengthOpt = None)
      val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
      val storage: VersionedAVLStorage[Digest32] = new VersionedAVLStorage(versionalStorage, np, settings)(Algos.hash)
      PersistentBatchAVLProver.create(bp, storage).getOrElse(throw new Error("Fatal: Failed to create persistent prover"))
    }
    new UtxoState(
      persistentProver,
      VersionTag @@ stateVersion,
      Height @@ stateHeight,
      versionalStorage,
      lastBlockTimestamp,
      nodeViewHolderRef,
      settings,
      statsSenderRef
    )
  }

  private def metadata(modId: VersionTag,
                       stateRoot: ADDigest,
                       height: Height,
                       blockTimestamp: Long): Seq[(Array[Byte], Array[Byte])] = {
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modId -> stateRoot
    val stateDigestIdIdxElem: (Digest32, VersionTag) = Algos.hash(stateRoot) -> modId
    val bestVersion: (Digest32, VersionTag) = bestVersionKey -> modId
    val stateHeight: (Digest32, Array[Byte]) = bestHeightKey -> Ints.toByteArray(height)
    val lastBlockTimestamp: (Digest32, Array[Byte]) = lastBlockTimeKey -> Longs.toByteArray(blockTimestamp)

    Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion, stateHeight, lastBlockTimestamp)
  }

  def genesis(boxes: List[EncryBaseBox],
              stateDir: File,
              nodeViewHolderRef: Option[ActorRef],
              settings: EncryAppSettings,
              statsSenderRef: Option[ActorRef]): UtxoState = {
    val p: BatchAVLProver[Digest32, HF] =
      new BatchAVLProver[Digest32, Algos.HF](keyLength = EncryBox.BoxIdSize, valueLengthOpt = None)
    boxes.foreach(b => p.performOneOperation(encry.avltree.Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))
    //check kind of storage
    val vldbInit = settings.storage.state match {
      case VersionalStorage.IODB =>
        logger.info("Init state with iodb storage")
        IODBWrapper(new LSMStore(stateDir, keepVersions = TestNetConstants.DefaultKeepVersions))
      case VersionalStorage.LevelDB =>
        logger.info("Init state with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(stateDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(300, 33), keySize = 33))
    }
    val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
    val storage: VersionedAVLStorage[Digest32] = new VersionedAVLStorage(vldbInit, np, settings)(Algos.hash)
    logger.info(s"Generating UTXO State with ${boxes.size} boxes")

    val persistentProver: encry.avltree.PersistentBatchAVLProver[Digest32, HF] = PersistentBatchAVLProver.create(
      p,
      storage,
      metadata(EncryState.genesisStateVersion, p.digest, TestNetConstants.PreGenesisHeight, 0L),
      paranoidChecks = true
    ).getOrElse(throw new Error("Fatal: Failed to create persistent prover"))

    new UtxoState(
      persistentProver,
      EncryState.genesisStateVersion,
      TestNetConstants.PreGenesisHeight,
      vldbInit,
      0L,
      nodeViewHolderRef,
      settings,
      statsSenderRef
    )
  }
}