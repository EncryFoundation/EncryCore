package encry.view.state

import java.io.File
import akka.actor.ActorRef
import com.google.common.primitives.{Ints, Longs}
import encry.CoreTaggedTypes.VersionTag
import encry.EncryApp.{settings, system}
import encry.avltree.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import encry.consensus.EncrySupplyController
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.Transaction
import encry.modifiers.mempool.Transaction.TransactionValidationException
import encry.modifiers.state.StateModifierDeserializer
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box._
import encry.utils.{BalanceCalculator, Logging}
import encry.settings.Constants
import encry.view.EncryNodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import encry.view.history.Height
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
                nodeViewHolderRef: Option[ActorRef])
  extends EncryState[UtxoState] with UtxoStateReader with Logging {

  import UtxoState.metadata

  override def rootHash: ADDigest = persistentProver.digest

  override def maxRollbackDepth: Int = Constants.Chain.MaxRollbackDepth

  private def onAdProofGenerated(proof: ADProofs): Unit = {
    if (nodeViewHolderRef.isEmpty) logWarn(s"Got proof while nodeViewHolderRef is empty")
    nodeViewHolderRef.foreach(_ ! LocallyGeneratedModifier(proof))
  }

  def applyBlockTransactions(blockTransactions: Seq[Transaction],
                             expectedDigest: ADDigest): Try[Unit] = {
    def applyTry(txs: Seq[Transaction], allowedOutputDelta: Amount = 0L): Try[Unit] =
      txs.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, tx) =>
        t.flatMap { _ =>
          validate(tx, allowedOutputDelta).flatMap { _ =>
            extractStateChanges(tx).operations.map(ADProofs.toModification)
              .foldLeft[Try[Option[ADValue]]](Success(None)) { case (tIn, m) =>
                tIn.flatMap(_ => persistentProver.performOneOperation(m))
            }
          }
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

    case block: EncryBlock =>
      logInfo(s"Applying block with header ${block.header.encodedId} to UtxoState with " +
        s"root hash ${Algos.encode(rootHash)} at height $height")

      applyBlockTransactions(block.payload.transactions, block.header.stateRoot).map { _ =>
        val meta: Seq[(Array[Byte], Array[Byte])] =
          metadata(VersionTag !@@ block.id, block.header.stateRoot, Height @@ block.header.height, block.header.timestamp)
        val proofBytes: SerializedAdProof = persistentProver.generateProofAndUpdateStorage(meta)
        val proofHash: Digest32 = ADProofs.proofDigest(proofBytes)

        if (block.adProofsOpt.isEmpty && settings.node.stateMode.isDigest) onAdProofGenerated(ADProofs(block.header.id, proofBytes))
        logInfo(s"Valid modifier ${block.encodedId} with header ${block.header.encodedId} applied to UtxoState with" +
          s" root hash ${Algos.encode(rootHash)}")

        if (!stateStore.get(ByteArrayWrapper(block.id)).exists(_.data sameElements block.header.stateRoot))
          throw new Exception("Storage kept roothash is not equal to the declared one.")
        else if (!stateStore.rollbackVersions().exists(_.data sameElements block.header.stateRoot))
          throw new Exception("Unable to apply modification properly.")
        else if (!(block.header.adProofsRoot sameElements proofHash))
          throw new Exception("Calculated proofHash is not equal to the declared one.")
        else if (!(block.header.stateRoot sameElements persistentProver.digest))
          throw new Exception("Calculated stateRoot is not equal to the declared one.")

        new UtxoState(persistentProver, VersionTag !@@ block.id, Height @@ block.header.height, stateStore, lastBlockTimestamp, nodeViewHolderRef)
      }.recoverWith[UtxoState] { case e =>
        logWarn(s"Failed to apply block with header ${block.header.encodedId} to UTXOState with root" +
          s" ${Algos.encode(rootHash)}: $e")
        Failure(e)
      }

    case header: EncryBlockHeader =>
      Success(new UtxoState(persistentProver, VersionTag !@@ header.id, height, stateStore, lastBlockTimestamp, nodeViewHolderRef))

    case _ => Failure(new Exception("Got Modifier of unknown type."))
  }

  def generateProofs(txs: Seq[Transaction]): Try[(SerializedAdProof, ADDigest)] = Try {
    logInfo(s"Generating proof for ${txs.length} transactions ...")
    val rootHash: ADDigest = persistentProver.digest
    if (txs.isEmpty) throw new Exception("Got empty transaction sequence")
    else if (!storage.version.exists(_.sameElements(rootHash)))
      throw new Exception(s"Invalid storage version: ${storage.version.map(Algos.encode)} != ${Algos.encode(rootHash)}")
    persistentProver.avlProver.generateProofForOperations(extractStateChanges(txs).operations.map(ADProofs.toModification))
  }.flatten.recoverWith[(SerializedAdProof, ADDigest)] { case e =>
    logWarn(s"Failed to generate ADProof cause $e")
    Failure(e)
  }

  override def rollbackTo(version: VersionTag): Try[UtxoState] = {
    logInfo(s"Rollback UtxoState to version ${Algos.encoder.encode(version)}")
    stateStore.get(ByteArrayWrapper(version)) match {
      case Some(v) =>
        val rollbackResult: Try[UtxoState] = persistentProver.rollback(ADDigest @@ v.data).map { _ =>
          val stateHeight: Int = stateStore.get(ByteArrayWrapper(UtxoState.bestHeightKey))
            .map(d => Ints.fromByteArray(d.data)).getOrElse(Constants.Chain.GenesisHeight)
          new UtxoState(persistentProver, version, Height @@ stateHeight, stateStore, lastBlockTimestamp, nodeViewHolderRef)
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

  def validate(tx: Transaction, allowedOutputDelta: Amount = 0L): Try[Unit] =
    tx.semanticValidity.map { _: Unit =>

      val stateView: EncryStateView = EncryStateView(height, lastBlockTimestamp, rootHash)

      val bxs: IndexedSeq[EncryBaseBox] = tx.inputs.flatMap(input => persistentProver.unauthenticatedLookup(input.boxId)
        .map(bytes => StateModifierDeserializer.parseBytes(bytes, input.boxId.head))
        .map(_.toOption -> input)).foldLeft(IndexedSeq[EncryBaseBox]()) { case (acc, (bxOpt, input)) =>
          (bxOpt, tx.defaultProofOpt) match {
            // If no `proofs` provided, then `defaultProof` is used.
            case (Some(bx), defaultProofOpt) if input.proofs.nonEmpty =>
              if (bx.proposition.canUnlock(Context(tx, bx, stateView), input.realContract,
                defaultProofOpt.map(input.proofs :+ _).getOrElse(input.proofs))) acc :+ bx else acc
            case (Some(bx), Some(defaultProof)) =>
              if (bx.proposition.canUnlock(Context(tx, bx, stateView), input.realContract, Seq(defaultProof))) acc :+ bx else acc
            case (Some(bx), defaultProofOpt) =>
              if (bx.proposition.canUnlock(Context(tx, bx, stateView), input.realContract,
                defaultProofOpt.map(Seq(_)).getOrElse(Seq.empty))) acc :+ bx else acc
            case _ => throw TransactionValidationException(s"Box(${Algos.encode(input.boxId)}) not found")
          }
        }

      val validBalance: Boolean = {
        val debitB: Map[TokenId, Amount] = BalanceCalculator.balanceSheet(bxs)
        val creditB: Map[TokenId, Amount] = {
          val balanceSheet: Map[TokenId, Amount] = BalanceCalculator.balanceSheet(tx.newBoxes, excludeTokenIssuance = true)
          val intrinsicBalance: Amount = balanceSheet.getOrElse(Constants.IntrinsicTokenId, 0L)
          balanceSheet.updated(Constants.IntrinsicTokenId, intrinsicBalance + tx.fee)
        }
        creditB.forall { case (tokenId, amount) =>
          if (ByteArrayWrapper(tokenId) == ByteArrayWrapper(Constants.IntrinsicTokenId))
            debitB.getOrElse(tokenId, 0L) + allowedOutputDelta >= amount
          else debitB.getOrElse(tokenId, 0L) >= amount
        }
      }

      if (!validBalance) throw TransactionValidationException(s"Non-positive balance in $tx")
    }

  def isValid(tx: Transaction, allowedOutputDelta: Amount = 0L): Boolean = validate(tx, allowedOutputDelta).isSuccess

  def filterValid(txs: Seq[Transaction]): Seq[Transaction] = txs.filter(tx => isValid(tx))
}

object UtxoState extends Logging {

  private val bestVersionKey: Digest32 = Algos.hash("best_state_version")

  private val bestHeightKey: Digest32 = Algos.hash("state_height")

  private val lastBlockTimeKey: Digest32 = Algos.hash("last_block_timestamp")

  def create(stateDir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
    val stateStore: LSMStore = new LSMStore(stateDir, keepVersions = Constants.DefaultKeepVersions)
    val stateVersion: Array[Byte] = stateStore.get(ByteArrayWrapper(bestVersionKey))
      .map(_.data).getOrElse(EncryState.genesisStateVersion)
    val stateHeight: Int = stateStore.get(ByteArrayWrapper(bestHeightKey))
      .map(d => Ints.fromByteArray(d.data)).getOrElse(Constants.Chain.PreGenesisHeight)
    val lastBlockTimestamp: Amount = stateStore.get(ByteArrayWrapper(lastBlockTimeKey))
      .map(d => Longs.fromByteArray(d.data)).getOrElse(0L)
    val persistentProver: encry.avltree.PersistentBatchAVLProver[Digest32, HF] = {
      val bp: encry.avltree.BatchAVLProver[Digest32, HF] = new encry.avltree.BatchAVLProver[Digest32, Algos.HF](keyLength = 32, valueLengthOpt = None)
      val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
      val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(stateStore, np)(Algos.hash)
      PersistentBatchAVLProver.create(bp, storage).getOrElse(throw new Error("Fatal: Failed to create persistent prover"))
    }
    new UtxoState(persistentProver, VersionTag @@ stateVersion, Height @@ stateHeight, stateStore, lastBlockTimestamp, nodeViewHolderRef)
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

  def genesis(boxes: List[EncryBaseBox], stateDir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
    val p: BatchAVLProver[Digest32, HF] =
      new BatchAVLProver[Digest32, Algos.HF](keyLength = EncryBox.BoxIdSize, valueLengthOpt = None)
    boxes.foreach(b => p.performOneOperation(encry.avltree.Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val stateStore: LSMStore = new LSMStore(stateDir, keepVersions = Constants.DefaultKeepVersions)
    val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
    val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(stateStore, np)(Algos.hash)
    logInfo(s"Generating UTXO State with ${boxes.size} boxes")

    val persistentProver: encry.avltree.PersistentBatchAVLProver[Digest32, HF] = PersistentBatchAVLProver.create(
      p, storage, metadata(EncryState.genesisStateVersion, p.digest, Constants.Chain.PreGenesisHeight, 0L), paranoidChecks = true
    ).getOrElse(throw new Error("Fatal: Failed to create persistent prover"))

    new UtxoState(persistentProver, EncryState.genesisStateVersion, Constants.Chain.PreGenesisHeight, stateStore, 0L, nodeViewHolderRef)
  }
}
