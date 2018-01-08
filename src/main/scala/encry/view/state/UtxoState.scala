package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.mempool.{EncryBaseTransaction, PaymentTransaction}
import encry.modifiers.state.TransactionValidator
import encry.modifiers.state.box._
import encry.settings.{Algos, Constants}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.VersionTag
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

import scala.util.{Failure, Success, Try}

class UtxoState(override val version: VersionTag,
                val store: Store,
                nodeViewHolderRef: Option[ActorRef])
  extends EncryState[UtxoState] with TransactionValidator with ScorexLogging {

  import UtxoState.metadata

  implicit val hf: Blake2b256Unsafe = new Blake2b256Unsafe
  // TODO: Note that Box has multiple subtypes of different lengths. Fix `valueSize`.
  private lazy val np = NodeParameters(keySize = 33, valueSize = AssetBoxSerializer.Length, labelSize = 32)
  protected lazy val storage = new VersionedIODBAVLStorage(store, np)

  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
    PersistentBatchAVLProver.create(
      new BatchAVLProver[Digest32, Blake2b256Unsafe](
        keyLength = 33, valueLengthOpt = Some(AssetBoxSerializer.Length)),
      storage).get

  override def maxRollbackDepth: Int = 10

  def typedBoxById(boxId: ADKey): Option[EncryBaseBox] = {
    boxId.head.toInt match {
      case 0 => persistentProver.unauthenticatedLookup(boxId)
        .map(OpenBoxSerializer.parseBytes).flatMap(_.toOption)
      case 1 => persistentProver.unauthenticatedLookup(boxId)
        .map(AssetBoxSerializer.parseBytes).flatMap(_.toOption)
    }
  }

  private def onAdProofGenerated(proof: ADProofs): Unit = {
    if(nodeViewHolderRef.isEmpty) log.warn("Got proof while nodeViewHolderRef is empty")
    nodeViewHolderRef.foreach(h => h ! LocallyGeneratedModifier(proof))
  }

  private[state] def checkTransactions(txs: Seq[EncryBaseTransaction],
                                       expectedDigest: ADDigest): Try[Unit] = Try {

    // Carries out an exhaustive txs validation.
    txs.foreach(tx => validate(tx))

    // Tries to apply operations to the state.
    getStateChanges(txs).operations.map(ADProofs.toModification)
      .foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, m) =>
      t.flatMap(_ => {
        persistentProver.performOneOperation(m)
      })
    }.get

    // Checks whether the outcoming result is the same as expected.
    if (!expectedDigest.sameElements(persistentProver.digest))
      Failure(new Error(s"Digest after txs application is wrong. ${Algos.encode(expectedDigest)} expected, " +
        s"${Algos.encode(persistentProver.digest)} given"))
  }

  // State transition function `APPLY(S,TX) -> S'`.
  override def applyModifier(mod: EncryPersistentModifier): Try[UtxoState] = mod match {
    case block: EncryBlock =>
      log.debug(s"Applying block with header ${block.header.encodedId} to UtxoState with " +
        s"root hash ${Algos.encode(rootHash)}")

      checkTransactions(block.payload.transactions, block.header.stateRoot) match {
        case Success(_) =>
          Try {
            val md = metadata(VersionTag @@ block.id, block.header.stateRoot)
            val proofBytes = persistentProver.generateProofAndUpdateStorage(md)
            val proofHash = ADProofs.proofDigest(proofBytes)

            if (block.adProofsOpt.isEmpty) onAdProofGenerated(ADProofs(block.header.id, proofBytes))
            log.info(s"Valid modifier ${block.encodedId} with header ${block.header.encodedId} applied to UtxoState with " +
              s"root hash ${Algos.encode(rootHash)}")

            if (!store.get(ByteArrayWrapper(block.id)).exists(_.data sameElements block.header.stateRoot))
              Failure(new Error("Storage kept roothash is not equal to the declared one."))
            else if (!store.rollbackVersions().exists(_.data sameElements block.header.stateRoot))
              Failure(new Error("Unable to apply modification properly."))
            else if (!(block.header.adProofsRoot sameElements proofHash))
              Failure(new Error("Calculated proofHash is not equal to the declared one."))
            else if (block.header.stateRoot sameElements persistentProver.digest)
              Failure(new Error("Calculated stateRoot is not equal to the declared one."))

            new UtxoState(VersionTag @@ block.id, store, nodeViewHolderRef)
          }
        case Failure(e) =>
          log.warn(s"Error while applying block with header ${block.header.encodedId} to UTXOState with root" +
            s" ${Algos.encode(rootHash)}: ", e)
          Failure(e)
      }

    case header: EncryBlockHeader =>
      Success(new UtxoState(VersionTag @@ header.id, this.store, nodeViewHolderRef))

    case _ => Failure(new Error("Got Modifier of unknown type."))
  }

  def proofsForTransactions(txs: Seq[EncryBaseTransaction]): Try[(SerializedAdProof, ADDigest)] = {

    def rollback(): Try[Unit] = persistentProver.rollback(rootHash)
      .ensuring(persistentProver.digest.sameElements(rootHash))

    Try {
      if (!(txs.isEmpty &&
        persistentProver.digest.sameElements(rootHash) &&
        storage.version.get.sameElements(rootHash) &&
        store.lastVersionID.get.data.sameElements(rootHash))) Failure(new Error(""))

      val mods = getStateChanges(txs).operations.map(ADProofs.toModification)
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
    store.get(ByteArrayWrapper(version)) match {
      case Some(hash) =>
        val rollbackResult = prover.rollback(ADDigest @@ hash.data).map { _ =>
          new UtxoState(version, store, nodeViewHolderRef) {
            override protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] = prover
          }
        }
        store.clean(Constants.keepVersions)
        rollbackResult
      case None =>
        Failure(new Error(s"Unable to get root hash at version ${Algos.encoder.encode(version)}"))
    }
  }

  override def rollbackVersions: Iterable[VersionTag] =
    persistentProver.storage.rollbackVersions.map(v =>
      VersionTag @@ store.get(ByteArrayWrapper(Algos.hash(v))).get.data)

  override lazy val rootHash: ADDigest = persistentProver.digest

  // TODO: Test.
  // TODO: BUG: Too many redundant tx signature validity checks here.
  // Carries out an exhaustive txs validation.
  override def validate(tx: EncryBaseTransaction): Try[Unit] = {

    tx.semanticValidity.get
    tx match {
      case tx: PaymentTransaction =>
        var inputsSum: Long = 0
        tx.unlockers.foreach { unl =>
          persistentProver.unauthenticatedLookup(unl.closedBoxId) match {
            case Some(data) =>
              unl.closedBoxId.head.toInt match {
                case 0 =>
                  OpenBoxSerializer.parseBytes(data) match {
                    case Success(box) => inputsSum = inputsSum + box.amount
                    case Failure(_) => Failure(new Error(s"Unable to parse Box referenced in TX ${tx.txHash}"))
                  }
                case 1 =>
                  AssetBoxSerializer.parseBytes(data) match {
                    case Success(box) =>
                      if (!unl.isValid(box.proposition, tx.senderProposition, tx.messageToSign))
                        Failure(new Error(s"Invalid unlocker for box referenced in $tx"))
                      inputsSum = inputsSum + box.amount
                    case Failure(_) => Failure(new Error(s"Unable to parse Box referenced in TX ${tx.txHash}"))
                  }
              }
            case None => Failure(new Error(s"Cannot find Box referenced in TX ${tx.txHash}"))
          }
        }
        if (tx.createBoxes.map(i => i._2).sum != inputsSum)
          Failure(new Error("Inputs total amount mismatches Output sum."))
      case _ => Failure(new Error("Got Modifier of unknown type."))
    }
    Success()
  }

  // Filters semantically valid and non conflicting transactions.
  // Picks the transaction with highest fee if conflict detected.
  // Note, this returns txs ordered by the amount of fee.
  // TODO: This method mainly invoked with ordered txs set from mempool,
  // TODO: so do we need to sort txs here?
  override def filterValid(txs: Seq[EncryBaseTransaction]): Seq[EncryBaseTransaction] = {
    val semValidTxs = super.filterValid(txs)
    semValidTxs
      .sortBy(tx => tx.fee)
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
    }
  }._1

  // TODO: Implement.
  def boxesOf(proposition: Proposition): Seq[Box[proposition.type]] = ???
}

object UtxoState {

  private lazy val bestVersionKey = Algos.hash("best_state_version")

  def create(dir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
    val store = new LSMStore(dir, keepVersions = Constants.keepVersions)
    val dbVersion = store.get(ByteArrayWrapper(bestVersionKey)).map( _.data)
    new UtxoState(VersionTag @@ dbVersion.getOrElse(EncryState.genesisStateVersion), store, nodeViewHolderRef)
  }

  private def metadata(modId: VersionTag, stateRoot: ADDigest): Seq[(Array[Byte], Array[Byte])] = {
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modId -> stateRoot
    val stateDigestIdIdxElem = Algos.hash(stateRoot) -> modId
    val bestVersion = bestVersionKey -> modId

    Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion)
  }

  def fromBoxHolder(bh: BoxHolder, dir: File, nodeViewHolderRef: Option[ActorRef]): UtxoState = {
    val p = new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32, valueLengthOpt = None)
    bh.sortedBoxes.foreach(b => p.performOneOperation(Insert(b.id, ADValue @@ b.bytes)).ensuring(_.isSuccess))

    val store = new LSMStore(dir, keepVersions = Constants.keepVersions)

    new UtxoState(EncryState.genesisStateVersion, store, nodeViewHolderRef) {
      override protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
        PersistentBatchAVLProver.create(
          p, storage, metadata(EncryState.genesisStateVersion, p.digest), paranoidChecks = true
        ).get.ensuring(_.digest sameElements storage.version.get)
    }
  }
}