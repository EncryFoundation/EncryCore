package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{CoinbaseTransaction, EncryBaseTransaction, PaymentTransaction}
import encry.modifiers.state.TransactionValidator
import encry.modifiers.state.box._
import encry.settings.{Algos, Constants}
import encry.view.state.index.StateIndexReader
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.utils.ScorexLogging
import scorex.core.{ModifierId, VersionTag}
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADValue, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

import scala.util.{Failure, Success, Try}

class UtxoState(override val version: VersionTag,
                override val stateStore: Store,
                override val indexStore: Store,
                nodeViewHolderRef: Option[ActorRef])
  extends EncryState[UtxoState] with UtxoStateReader with StateIndexReader with TransactionValidator {

  import UtxoState.metadata

  override def maxRollbackDepth: Int = 10

  private def onAdProofGenerated(proof: ADProofs): Unit = {
    if(nodeViewHolderRef.isEmpty) log.warn("Got proof while nodeViewHolderRef is empty")
    nodeViewHolderRef.foreach(h => h ! LocallyGeneratedModifier(proof))
  }

  // TODO: Make sure all errors are being caught properly.
  private[state] def applyTransactions(txs: Seq[EncryBaseTransaction],
                                       expectedDigest: ADDigest, modId: ModifierId): Try[Unit] = Try {

    def rollback(): Try[Unit] = persistentProver.rollback(rootHash)
      .ensuring(persistentProver.digest.sameElements(rootHash))

    var appliedModCounter: Int = 0

    txs.foreach { tx =>
      // Carries out an exhaustive txs validation and then tries to apply it.
      if (validate(tx).isSuccess) {
        getStateChanges(tx).operations.map(ADProofs.toModification)
          .foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, m) =>
          t.flatMap { _ =>
            appliedModCounter += 1
            persistentProver.performOneOperation(m)
          }
        }
      } else {
        if (appliedModCounter > 0) rollback()
        throw new Error(s"Error while applying modifier $tx.")
      }
    }

    // Checks whether the outcoming result is the same as expected.
    if (!expectedDigest.sameElements(persistentProver.digest))
      throw new Error(s"Digest after txs application is wrong. ${Algos.encode(expectedDigest)} expected, " +
        s"${Algos.encode(persistentProver.digest)} given")
  }

  // State transition function `APPLY(S,TX) -> S'`.
  override def applyModifier(mod: EncryPersistentModifier): Try[UtxoState] = mod match {

    case block: EncryBlock =>
      log.debug(s"Applying block with header ${block.header.encodedId} to UtxoState with " +
        s"root hash ${Algos.encode(rootHash)}")

      applyTransactions(block.payload.transactions, block.header.stateRoot, block.id).map { _: Unit =>
        val md = metadata(VersionTag @@ block.id, block.header.stateRoot)
        val proofBytes = persistentProver.generateProofAndUpdateStorage(md)
        val proofHash = ADProofs.proofDigest(proofBytes)

        if (block.adProofsOpt.isEmpty) onAdProofGenerated(ADProofs(block.header.id, proofBytes))
        log.info(s"Valid modifier ${block.encodedId} with header ${block.header.encodedId} applied to UtxoState with " +
          s"root hash ${Algos.encode(rootHash)}")

        if (!stateStore.get(ByteArrayWrapper(block.id)).exists(_.data sameElements block.header.stateRoot))
          throw new Error("Storage kept roothash is not equal to the declared one.")
        else if (!stateStore.rollbackVersions().exists(_.data sameElements block.header.stateRoot))
          throw new Error("Unable to apply modification properly.")
        else if (!(block.header.adProofsRoot sameElements proofHash))
          throw new Error("Calculated proofHash is not equal to the declared one.")
        else if (!(block.header.stateRoot sameElements persistentProver.digest))
          throw new Error("Calculated stateRoot is not equal to the declared one.")

        // Update state index.
        updateIndexOn(block)

        new UtxoState(VersionTag @@ block.id, stateStore, indexStore, nodeViewHolderRef)
      }.recoverWith[UtxoState] { case e =>
        log.warn(s"Error while applying block with header ${block.header.encodedId} to UTXOState with root" +
          s" ${Algos.encode(rootHash)}: ", e)
        Failure(e)
      }

    case header: EncryBlockHeader =>
      Success(new UtxoState(VersionTag @@ header.id, stateStore, indexStore, nodeViewHolderRef))

    case _ => Failure(new Error("Got Modifier of unknown type."))
  }

  def proofsForTransactions(txs: Seq[EncryBaseTransaction]): Try[(SerializedAdProof, ADDigest)] = {

    def rollback(): Try[Unit] = Try(
      persistentProver.rollback(rootHash).ensuring(_.isSuccess && persistentProver.digest.sameElements(rootHash))
    ).flatten

    Try {
      assert(txs.nonEmpty, "Empty transaction sequence passed.")

      if (!(persistentProver.digest.sameElements(rootHash) &&
        storage.version.get.sameElements(rootHash) &&
        stateStore.lastVersionID.get.data.sameElements(rootHash)))
        Failure(new Error("Bad state version."))

      val mods = getAllStateChanges(txs).operations.map(ADProofs.toModification)
      //todo .get
      mods.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, m) =>
        t.flatMap(_ => {
          val opRes = persistentProver.performOneOperation(m)
          if (opRes.isFailure) log.warn(s"modification: $m, failure $opRes")
          opRes
        })
      }.get

      val proof = persistentProver.generateProofAndUpdateStorage()
      val digest = persistentProver.digest

      proof -> digest
    } match {
      case Success(res) => rollback().map(_ => res)
      case Failure(e) => rollback().flatMap(_ => Failure(e))
    }
  }

  override def rollbackTo(version: VersionTag): Try[UtxoState] = {
    val prover = persistentProver
    log.info(s"Rollback UtxoState to version ${Algos.encoder.encode(version)}")
    stateStore.get(ByteArrayWrapper(version)) match {
      case Some(hash) =>
        val rollbackResult = prover.rollback(ADDigest @@ hash.data).map { _ =>
          new UtxoState(version, stateStore, indexStore, nodeViewHolderRef) {
            override protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] = prover
          }
        }
        stateStore.clean(Constants.keepVersions)
        rollbackResult
      case None =>
        Failure(new Error(s"Unable to get root hash at version ${Algos.encoder.encode(version)}"))
    }
  }

  override def rollbackVersions: Iterable[VersionTag] =
    persistentProver.storage.rollbackVersions.map(v =>
      VersionTag @@ stateStore.get(ByteArrayWrapper(Algos.hash(v))).get.data)

  override lazy val rootHash: ADDigest = persistentProver.digest

  // TODO: Test.
  // TODO: OPTIMISATION: Too many redundant signature validity checks here.
  // Carries out an exhaustive tx validation.
  override def validate(tx: EncryBaseTransaction): Try[Unit] = Try {

    tx.semanticValidity.get
    tx match {
      case tx: PaymentTransaction =>
        var inputsSumCounter: Long = 0
        tx.useBoxes.foreach { bxId =>
          persistentProver.unauthenticatedLookup(bxId) match {
            case Some(data) =>
              bxId.head match {
                case OpenBox.typeId =>
                  OpenBoxSerializer.parseBytes(data) match {
                    case Success(box) =>
                      inputsSumCounter += box.amount
                    case Failure(_) =>
                      throw new Error(s"Unable to parse Box referenced in TX ${tx.txHash}")
                  }
                case AssetBox.typeId =>
                  AssetBoxSerializer.parseBytes(data) match {
                    case Success(box) =>
                      if (box.unlockTry(tx).isFailure)
                        throw new Error(s"Invalid unlocker for box referenced in $tx")
                      inputsSumCounter += box.amount
                    case Failure(_) =>
                      throw new Error(s"Unable to parse Box referenced in TX ${tx.txHash}")
                  }
                case _ => throw new Exception("Got Modifier of unknown type.")
              }
            case None =>
              throw new Error(s"Cannot find Box referenced in TX ${tx.txHash}")
          }
        }
        if (tx.createBoxes.map(i => i._2).sum > inputsSumCounter)
          throw new Error("Inputs total amount mismatches Output sum.")

      case tx: CoinbaseTransaction =>
        tx.useBoxes.foreach { bxId =>
          persistentProver.unauthenticatedLookup(bxId) match {
            case Some(data) =>
              bxId.head match {
                case OpenBox.typeId =>
                  OpenBoxSerializer.parseBytes(data) match {
                    case Success(box) =>
                      // TODO: How to get `bestHeaderHeight` to compare with `box.proposition.height`?
                      if (box.proposition.height > 0)
                        throw new Error(s"Box referenced in tx: $tx is disallowed to be spent at current height.")
                    case Failure(_) =>
                      throw new Error(s"Unable to parse Box referenced in TX ${tx.txHash}")
                  }
                case _ =>
                  throw new Error("Got Modifier of unknown type.")
              }
            case None =>
              throw new Exception(s"Cannot find Box referenced in TX ${tx.txHash}")
          }
        }
      case _ => throw new Error("Got Modifier of unknown type.")
    }
  }

  // Filters semantically valid and non conflicting transactions.
  // Picks the transaction with highest fee if conflict detected.
  // Note, this returns txs ordered chronologically.
  // TODO: OPTIMISATION: Too many sorting here.
  override def filterValid(txs: Seq[EncryBaseTransaction]): Seq[EncryBaseTransaction] =
    super.filterValid(txs.sortBy(_.timestamp))
      .sortBy(_.fee)
      .reverse
      .foldLeft((Seq[EncryBaseTransaction](), Set[ByteArrayWrapper]())) { case ((nonConflictTxs, bxs), tx) =>
      tx match {
        case tx: PaymentTransaction =>
          val bxsRaw = tx.useBoxes.map(ByteArrayWrapper.apply)
          if (bxsRaw.forall(k => !bxs.contains(k)) && bxsRaw.size == bxsRaw.toSet.size) {
            (nonConflictTxs :+ tx) -> (bxs ++ bxsRaw)
          } else {
            (nonConflictTxs, bxs)
          }
      }
    }._1.sortBy(_.timestamp)
}

object UtxoState extends ScorexLogging {

  private lazy val bestVersionKey = Algos.hash("best_state_version")

  def create(stateDir: File, indexDir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
    val stateStore = new LSMStore(stateDir, keepVersions = Constants.keepVersions)
    val indexStore = new LSMStore(indexDir,
      keySize = PublicKey25519Proposition.AddressLength, keepVersions = Constants.keepVersions)
    val dbVersion = stateStore.get(ByteArrayWrapper(bestVersionKey)).map( _.data)
    new UtxoState(VersionTag @@ dbVersion.getOrElse(EncryState.genesisStateVersion),
      stateStore, indexStore, nodeViewHolderRef)
  }

  private def metadata(modId: VersionTag, stateRoot: ADDigest): Seq[(Array[Byte], Array[Byte])] = {
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modId -> stateRoot
    val stateDigestIdIdxElem = Algos.hash(stateRoot) -> modId
    val bestVersion = bestVersionKey -> modId

    Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion)
  }

  def fromBoxHolder(bh: BoxHolder, stateDir: File,
                    indexDir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
    val p = new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32, valueLengthOpt = None)
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val stateStore = new LSMStore(stateDir, keepVersions = Constants.keepVersions)
    val indexStore = new LSMStore(indexDir,
      keySize = PublicKey25519Proposition.AddressLength, keepVersions = Constants.keepVersions)

    log.info(s"Generating UTXO State from BH with ${bh.boxes.size} boxes")

    new UtxoState(EncryState.genesisStateVersion, stateStore, indexStore, nodeViewHolderRef) {
      override protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
        PersistentBatchAVLProver.create(
          p, storage, metadata(EncryState.genesisStateVersion, p.digest), paranoidChecks = true
        ).get.ensuring(_.digest sameElements storage.version.get)
    }
  }
}
