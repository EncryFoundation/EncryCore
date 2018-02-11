package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{AddPubKeyInfoTransaction, CoinbaseTransaction, EncryBaseTransaction, PaymentTransaction}
import encry.modifiers.state.box._
import encry.modifiers.state.box.proposition.HeightProposition
import encry.settings.{Algos, Constants}
import encry.view.history.Height
import encry.view.state.index.StateIndexManager
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.LocalInterface.LocallyGeneratedModifier
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
  extends EncryState[UtxoState] with UtxoStateReader with StateIndexManager with TransactionValidator {

  import UtxoState.metadata

  override def maxRollbackDepth: Int = 10

  private def onAdProofGenerated(proof: ADProofs): Unit = {
    if(nodeViewHolderRef.isEmpty) log.warn("Got proof while nodeViewHolderRef is empty")
    nodeViewHolderRef.foreach(_ ! LocallyGeneratedModifier(proof))
  }

  // TODO: Make sure all errors are being caught properly.
  private[state] def applyTransactions(txs: Seq[EncryBaseTransaction],
                                       expectedDigest: ADDigest): Try[Unit] = Try {

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

    log.debug(s"$appliedModCounter modifications applied")

    // Checks whether the outcoming result is the same as expected.
    if (!expectedDigest.sameElements(persistentProver.digest))
      throw new Error(s"Digest after txs application is wrong. ${Algos.encode(expectedDigest)} expected, " +
        s"${Algos.encode(persistentProver.digest)} given")
  }

  // State transition function `APPLY(S,TX) -> S'`.
  override def applyModifier(mod: EncryPersistentModifier): Try[UtxoState] = mod match {

    case block: EncryBlock =>
      log.debug(s"Applying block with header ${block.header.encodedId} to UtxoState with " +
        s"root hash ${Algos.encode(rootHash)} at height $stateHeight")

      applyTransactions(block.payload.transactions, block.header.stateRoot).map { _: Unit =>
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
        updateIndex(block)

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
      assert(txs.nonEmpty, "Got empty transaction sequence.")

      if (!(persistentProver.digest.sameElements(rootHash) &&
        storage.version.get.sameElements(rootHash) &&
        stateStore.lastVersionID.get.data.sameElements(rootHash))) Failure(new Error("Bad state version."))

      val mods = getAllStateChanges(txs).operations.map(ADProofs.toModification)

      // TODO: Refactoring.
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
      case Success(result) => rollback().map(_ => result)
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

  /**
    * Carries out an exhaustive validation of the given transaction.
    *
    * Transaction validation algorithm:
    * 0. Check semantic validity of transaction
    *    For each box referenced in transaction:
    * 1. Check if box is in the state
    * 2. Parse box from the state storage
    * 3. Try to unlock the box, providing appropriate context
    * 4. Make sure inputs.sum >= outputs.sum
   */
  override def validate(tx: EncryBaseTransaction): Try[Unit] =
    tx.semanticValidity.map { _: Unit =>

      implicit val context: Option[Context] = Some(Context(stateHeight))

      val bxs = tx.useBoxes.map(bxId => persistentProver.unauthenticatedLookup(bxId)
        .map(bytes => StateModifierDeserializer.parseBytes(bytes, bxId.head))
        .flatMap(_.toOption)).foldLeft(IndexedSeq[EncryBaseBox]())((acc, bxOpt) => bxOpt match {
          case Some(bx) if bx.unlockTry(tx, None).isSuccess => acc :+ bx
          case _ => acc
        })

      val debit = bxs.foldLeft(0L)((acc, bx) => bx match {
        case acbx: AmountCarryingBox => acc + acbx.amount
        case _ => acc
      })

      val credit = tx match {
        case tx: PaymentTransaction => tx.createBoxes.foldLeft(0L)(_ + _._2) + tx.fee
        case _ => tx.fee
      }

      if (bxs.size < tx.useBoxes.size) throw new Error(s"Failed to spend some boxes referenced in $tx")
      else if (debit < credit) throw new Error(s"Non-positive balance in $tx")
    }

  /**
    * Filters semantically valid and non conflicting transactions.
    * Picks the transaction with highest fee if conflict detected.
    * Note, this returns txs ordered chronologically.
    */
  // TODO: OPTIMISATION: Too many sorting here.
  override def filterValid(txs: Seq[EncryBaseTransaction]): Seq[EncryBaseTransaction] =
    super.filterValid(txs.sortBy(_.timestamp))
      .sortBy(_.fee)
      .reverse
      .foldLeft((Seq[EncryBaseTransaction](), Set[ByteArrayWrapper]())) { case ((nonConflictTxs, bxs), tx) =>
      tx match {
        case tx: EncryBaseTransaction =>
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
    val stateStore = new LSMStore(stateDir, keepVersions = Constants.Store.stateKeepVersions)
    val indexStore = new LSMStore(indexDir, keepVersions = Constants.Store.indexKeepVersions)
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
    val p = new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = EncryBox.BoxIdSize, valueLengthOpt = None)
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val stateStore = new LSMStore(stateDir, keepVersions = Constants.Store.stateKeepVersions)
    val indexStore = new LSMStore(indexDir, keepVersions = Constants.Store.indexKeepVersions)

    log.info(s"Generating UTXO State from BH with ${bh.boxes.size} boxes")

    new UtxoState(EncryState.genesisStateVersion, stateStore, indexStore, nodeViewHolderRef) {
      override protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
        PersistentBatchAVLProver.create(
          p, storage, metadata(EncryState.genesisStateVersion, p.digest), paranoidChecks = true
        ).get.ensuring(_.digest sameElements storage.version.get)
    }
  }

  def newOpenBoxAt(height: Height, seed: Long): OpenBox = {
    val perBlockEmissionAmount = 2000L
    OpenBox(HeightProposition(Height @@ (height + Constants.coinbaseHeightLock)),
      seed * height, perBlockEmissionAmount)
  }
}
