package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.{EncryBaseTransaction, EncryPaymentTransaction}
import encry.modifiers.state.box._
import encry.modifiers.state.TransactionValidator
import encry.settings.{Algos, Constants}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.{EphemerealNodeViewModifier, VersionTag}
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, SerializedAdProof}
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

import scala.util.{Failure, Success, Try}

class UtxoState(override val version: VersionTag,
                val store: Store,
                /*nodeViewHolderRef: Option[ActorRef]*/)
  extends EncryState[UtxoState] with TransactionValidator with ScorexLogging {

  import UtxoState.metadata

  implicit val hf = new Blake2b256Unsafe
  private lazy val np = NodeParameters(keySize = 32, valueSize = EncryPaymentBoxSerializer.Length, labelSize = 32)
  protected lazy val storage = new VersionedIODBAVLStorage(store, np)

  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
    PersistentBatchAVLProver.create(
      new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32,
        valueLengthOpt = Some(EncryPaymentBoxSerializer.Length)),
      storage,
    ).get

  override def maxRollbackDepth: Int = 10

  // TODO: Fix return type
  def boxById(boxType: Any, boxId: ADKey): Option[Any] = {
    boxType match {
      case _: EncryPaymentBox => persistentProver.unauthenticatedLookup(boxId)
        .map(EncryPaymentBoxSerializer.parseBytes).flatMap(_.toOption)
    }
  }

  // Extracts `state changes` from the given sequence of transactions.
  def boxChanges(txs: Seq[EncryBaseTransaction]): EncryBoxStateChanges = {
    // Use neither `.filter` nor any validity checks here!
    // This method should be invoked when all txs are already validated.
    EncryBoxStateChanges(
      txs.flatMap { tx =>
        tx match {
          case tx: EncryPaymentTransaction =>
            tx.unlockers.map( unl => Removal(unl.closedBoxId)) ++
              tx.newBoxes.map( bx => Insertion(bx) )

//        case tx: AnotherTypeTransaction => ...
        }
      }
    )
  }

  private def onAdProofGenerated(proof: ADProofs): Unit = {
    // TODO: Notify `NodeView` of proof generation.
    //    if(nodeViewHolderRef.isEmpty) log.warn("Got proof while nodeViewHolderRef is empty")
    //    nodeViewHolderRef.foreach(h => h ! LocallyGeneratedModifier(proof))
  }

  private[state] def checkTransactions(txs: Seq[EncryBaseTransaction], expectedDigest: ADDigest): Try[Unit] = Try {

    // Carries out an exhaustive txs validation.
    txs.foreach { tx =>
      tx.semanticValidity.get
      tx match {
        case tx: EncryPaymentTransaction =>
          var inputsSum: Long = 0
          tx.useOutputs.foreach { key =>
            persistentProver.unauthenticatedLookup(key) match {
              case Some(data) =>
                EncryPaymentBoxSerializer.parseBytes(data).get match {
                  case box: EncryPaymentBox =>
                    if (box.proposition.address == tx.senderProposition.address)
                      inputsSum = inputsSum + box.body.amount
                    else Failure(new Error(""))
                  case _ => Failure(new Error(s"Cannot parse Box referenced in TX ${tx.txHash}"))
                }
              case None => Failure(new Error(s"Cannot parse Box referenced in TX ${tx.txHash}"))
            }
          }
          if (tx.createOutputs.map(i => i._2).sum != inputsSum) Failure(new Error("Inputs total amount overflow."))
        case _ => Failure(new Error("Got Modifier of unknown type."))
      }
    }

    // Tries to apply operations to the state.
    boxChanges(txs).operations.map(ADProofs.toModification)
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

  // Dispatches applying `Modifier` of particular type.
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

            // TODO: Describe errors properly.
            if (block.adProofsOpt.isEmpty) onAdProofGenerated(ADProofs(block.header.id, proofBytes))
            log.info(s"Valid modifier ${block.encodedId} with header ${block.header.encodedId} applied to UtxoState with " +
              s"root hash ${Algos.encode(rootHash)}")
            if (!store.get(ByteArrayWrapper(block.id)).exists(_.data sameElements block.header.stateRoot))
              Failure(new Error("Unable to apply modification correctly."))
            if (!store.rollbackVersions().exists(_.data sameElements block.header.stateRoot))
              Failure(new Error("Unable to apply modification correctly."))
            if (!block.header.adProofsRoot.sameElements(proofHash))
              Failure(new Error("Unable to apply modification correctly."))
            if (block.header.stateRoot sameElements persistentProver.digest)
              Failure(new Error("Unable to apply modification correctly."))

            new UtxoState(VersionTag @@ block.id, store)
          }
        case Failure(e) =>
          log.warn(s"Error while applying block with header ${block.header.encodedId} to UTXOState with root" +
            s" ${Algos.encode(rootHash)}: ", e)
          Failure(e)
      }
    case _ => Failure(new Error("Got Modifier of unknown type."))
  }

  def proofsForTransactions(txs: Seq[EncryBaseTransaction]): Try[(SerializedAdProof, ADDigest)] = {

    def rollback(): Try[Unit] = Try(
      persistentProver.rollback(rootHash).ensuring(_.isSuccess && persistentProver.digest.sameElements(rootHash))
    ).flatten

    Try {
      if (!(txs.isEmpty &&
        persistentProver.digest.sameElements(rootHash) &&
        storage.version.get.sameElements(rootHash) &&
        store.lastVersionID.get.data.sameElements(rootHash))) Failure(new Error(""))

      val mods = boxChanges(txs).operations.map(ADProofs.toModification)
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

  //TODO: implement
  override def rollbackTo(version: VersionTag): Try[UtxoState] = {
    val prover = persistentProver
    log.info(s"Rollback UtxoState to version ${Algos.encoder.encode(version)}")
    store.get(ByteArrayWrapper(version)) match {
      case Some(hash) =>
        val rollbackResult = prover.rollback(ADDigest @@ hash.data).map { _ =>
          new UtxoState(version, store/*, nodeViewHolderRef*/) {
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

  override def validate(tx: EphemerealNodeViewModifier): Try[Unit] = {
    tx match {
      case tx: EncryPaymentTransaction =>
        tx.semanticValidity
        if (!tx.useOutputs.forall(key => persistentProver.unauthenticatedLookup(key).isDefined))
          Failure(new Error(s"Non-existent box referenced <tx: ${tx.txHash}>"))
      case _ => Failure(new Error("Got unknown modifier."))
    }
    Success()
  }

  override def boxesOf(proposition: Proposition): Seq[Box[proposition.type]] = ???

}

object UtxoState {

  private lazy val bestVersionKey = Algos.hash("best state version") // TODO: ???

  def create(dir: File): UtxoState = {
    val store = new LSMStore(dir, keepVersions = 20) // todo: magic number, move to settings
    val dbVersion = store.get(ByteArrayWrapper(bestVersionKey)).map( _.data)
    new UtxoState(VersionTag @@ dbVersion.getOrElse(EncryState.genesisStateVersion), store)
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

    new UtxoState(EncryState.genesisStateVersion, store/*, nodeViewHolderRef*/) {
      override protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
        PersistentBatchAVLProver.create(p,
                                        storage,
                                        metadata(EncryState.genesisStateVersion, p.digest),
                                        paranoidChecks = true).get

      assert(persistentProver.digest.sameElements(storage.version.get))
    }
  }
}