package encry.view.state

import java.io.File
import akka.actor.ActorRef
import com.google.common.primitives.{Ints, Longs}
import com.typesafe.scalalogging.StrictLogging
import encry.avltree.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import encry.consensus.EncrySupplyController
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.{ADProofs, Block, Header}
import encry.modifiers.mempool.Transaction
import encry.modifiers.state.StateModifierSerializer
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.modifiers.state.box._
import encry.settings.{Constants, EncryAppSettings}
import encry.utils.CoreTaggedTypes.VersionTag
import encry.utils.BalanceCalculator
import encry.stats.StatsSender.TxsInBlock
import encry.validation.{MalformedModifierError, ValidationResult}
import encry.validation.ValidationResult.{Invalid, Valid}
import encry.view.EncryNodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import encry.view.history.History.Height
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.encryfoundation.common.Algos
import org.encryfoundation.common.Algos.HF
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ADValue, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scala.util.{Failure, Success, Try}

class UtxoState(override val persistentProver: encry.avltree.PersistentBatchAVLProver[Digest32, HF],
                override val version: VersionTag,
                override val height: Height,
                override val stateStore: Store,
                val lastBlockTimestamp: Long,
                nodeViewHolderRef: Option[ActorRef],
                settings: EncryAppSettings,
                statsSenderRef: Option[ActorRef])
  extends EncryState[UtxoState] with UtxoStateReader with StrictLogging {

  import UtxoState.metadata

  override def rootHash: ADDigest = persistentProver.digest

  def maxRollbackDepth: Int = Constants.Chain.MaxRollbackDepth

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
            extractStateChanges(tx).operations.map(ADProofs.toModification)
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

  override def applyModifier(mod: EncryPersistentModifier): Try[UtxoState] = mod match {

    case block: Block =>
      logger.info(s"Applying block with header ${block.header.encodedId} to UtxoState with " +
        s"root hash ${Algos.encode(rootHash)} at height $height")
      statsSenderRef.foreach(_ ! TxsInBlock(block.payload.transactions.size))
      applyBlockTransactions(block.payload.transactions, block.header.stateRoot).map { _ =>
        val meta: Seq[(Array[Byte], Array[Byte])] = metadata(
          VersionTag !@@ block.id,
          block.header.stateRoot,
          Height @@ block.header.height,
          block.header.timestamp
        )
        val proofBytes: SerializedAdProof = persistentProver.generateProofAndUpdateStorage(meta)
        val proofHash: Digest32 = ADProofs.proofDigest(proofBytes)

        if (block.adProofsOpt.isEmpty && settings.node.stateMode.isDigest)
          onAdProofGenerated(ADProofs(block.header.id, proofBytes))
        logger.info(s"Valid modifier ${block.encodedId} with header ${block.header.encodedId} applied to UtxoState with" +
          s" root hash ${Algos.encode(rootHash)}")

        if (!stateStore.get(ByteArrayWrapper(block.id)).exists(_.data sameElements block.header.stateRoot))
          throw new Exception("Storage kept roothash is not equal to the declared one.")
        else if (!stateStore.rollbackVersions().exists(_.data sameElements block.header.stateRoot))
          throw new Exception("Unable to apply modification properly.")
        else if (!(block.header.adProofsRoot sameElements proofHash))
          throw new Exception("Calculated proofHash is not equal to the declared one.")
        else if (!(block.header.stateRoot sameElements persistentProver.digest))
          throw new Exception("Calculated stateRoot is not equal to the declared one.")

        new UtxoState(
          persistentProver,
          VersionTag !@@ block.id,
          Height @@ block.header.height,
          stateStore,
          lastBlockTimestamp,
          nodeViewHolderRef,
          settings,
          statsSenderRef
        )
      }.recoverWith[UtxoState] { case e =>
        logger.info(s"Failed to apply block with header ${block.header.encodedId} to UTXOState with root" +
          s" ${Algos.encode(rootHash)}: $e")
        Failure(e)
      }

    case header: Header =>
      Success(new UtxoState(
        persistentProver,
        VersionTag !@@ header.id,
        height,
        stateStore,
        lastBlockTimestamp,
        nodeViewHolderRef,
        settings,
        statsSenderRef
      ))

    case _ => Failure(new Exception("Got Modifier of unknown type."))
  }

  def generateProofs(txs: Seq[Transaction]): Try[(SerializedAdProof, ADDigest)] = Try {
    logger.info(s"Generating proof for ${txs.length} transactions ...")
    val rootHash: ADDigest = persistentProver.digest
    if (txs.isEmpty) throw new Exception("Got empty transaction sequence")
    else if (!storage.version.exists(_.sameElements(rootHash)))
      throw new Exception(s"Invalid storage version: ${storage.version.map(Algos.encode)} != ${Algos.encode(rootHash)}")
    persistentProver.avlProver.generateProofForOperations(extractStateChanges(txs).operations.map(ADProofs.toModification))
  }.flatten.recoverWith[(SerializedAdProof, ADDigest)] { case e =>
    logger.info(s"Failed to generate ADProof cause $e")
    Failure(e)
  }

  override def rollbackTo(version: VersionTag): Try[UtxoState] = {
    logger.info(s"Rollback UtxoState to version ${Algos.encoder.encode(version)}")
    stateStore.get(ByteArrayWrapper(version)) match {
      case Some(v) =>
        val rollbackResult: Try[UtxoState] = persistentProver.rollback(ADDigest @@ v.data).map { _ =>
          val stateHeight: Int = stateStore.get(ByteArrayWrapper(UtxoState.bestHeightKey))
            .map(d => Ints.fromByteArray(d.data)).getOrElse(Constants.Chain.GenesisHeight)
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
        stateStore.clean(Constants.DefaultKeepVersions)
        rollbackResult
      case None =>
        Failure(new Error(s"Unable to get root hash at version ${Algos.encoder.encode(version)}"))
    }
  }

  override def rollbackVersions: Iterable[VersionTag] =
    persistentProver.storage.rollbackVersions.map(v =>
      VersionTag @@ stateStore.get(ByteArrayWrapper(Algos.hash(v))).get.data)

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
              if (bx.proposition.canUnlock(Context(tx, bx, stateView), input.realContract,
                defaultProofOpt.map(input.proofs :+ _).getOrElse(input.proofs))) acc :+ bx else acc
            case (Some(bx), Some(defaultProof)) =>
              if (bx.proposition.canUnlock(Context(tx, bx, stateView), input.realContract, Seq(defaultProof)))
                acc :+ bx else acc
            case (Some(bx), defaultProofOpt) =>
              if (bx.proposition.canUnlock(Context(tx, bx, stateView), input.realContract,
                defaultProofOpt.map(Seq(_)).getOrElse(Seq.empty))) acc :+ bx else acc
            case _ => acc
          }
        }

      val validBalance: Boolean = {
        val debitB: Map[String, Amount] = BalanceCalculator.balanceSheet(bxs).map {
          case (tokenId, amount) => Algos.encode(tokenId) -> amount
        }
        val creditB: Map[String, Amount] = {
          val balanceSheet: Map[TokenId, Amount] = BalanceCalculator.balanceSheet(tx.newBoxes, excludeTokenIssuance = true)
          val intrinsicBalance: Amount = balanceSheet.getOrElse(Constants.IntrinsicTokenId, 0L)
          balanceSheet.updated(Constants.IntrinsicTokenId, intrinsicBalance + tx.fee)
        }.map { case (tokenId, amount) => Algos.encode(tokenId) -> amount }
        creditB.forall { case (tokenId, amount) =>
          if (tokenId == Algos.encode(Constants.IntrinsicTokenId))
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
    val stateStore: LSMStore = new LSMStore(stateDir, keepVersions = Constants.DefaultKeepVersions)
    val stateVersion: Array[Byte] = stateStore.get(ByteArrayWrapper(bestVersionKey))
      .map(_.data).getOrElse(EncryState.genesisStateVersion)
    val stateHeight: Int = stateStore.get(ByteArrayWrapper(bestHeightKey))
      .map(d => Ints.fromByteArray(d.data)).getOrElse(Constants.Chain.PreGenesisHeight)
    val lastBlockTimestamp: Amount = stateStore.get(ByteArrayWrapper(lastBlockTimeKey))
      .map(d => Longs.fromByteArray(d.data)).getOrElse(0L)
    val persistentProver: encry.avltree.PersistentBatchAVLProver[Digest32, HF] = {
      val bp: encry.avltree.BatchAVLProver[Digest32, HF] =
        new encry.avltree.BatchAVLProver[Digest32, Algos.HF](keyLength = 32, valueLengthOpt = None)
      val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
      val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(stateStore, np)(Algos.hash)
      PersistentBatchAVLProver.create(bp, storage).getOrElse(throw new Error("Fatal: Failed to create persistent prover"))
    }
    new UtxoState(
      persistentProver,
      VersionTag @@ stateVersion,
      Height @@ stateHeight,
      stateStore,
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

    val stateStore: LSMStore = new LSMStore(stateDir, keepVersions = Constants.DefaultKeepVersions)
    val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
    val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(stateStore, np)(Algos.hash)
    logger.info(s"Generating UTXO State with ${boxes.size} boxes")

    val persistentProver: encry.avltree.PersistentBatchAVLProver[Digest32, HF] = PersistentBatchAVLProver.create(
      p,
      storage,
      metadata(EncryState.genesisStateVersion, p.digest, Constants.Chain.PreGenesisHeight, 0L),
      paranoidChecks = true
    ).getOrElse(throw new Error("Fatal: Failed to create persistent prover"))

    new UtxoState(
      persistentProver,
      EncryState.genesisStateVersion,
      Constants.Chain.PreGenesisHeight,
      stateStore,
      0L,
      nodeViewHolderRef,
      settings,
      statsSenderRef
    )
  }
}
