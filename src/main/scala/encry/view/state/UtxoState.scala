package encry.view.state

import akka.actor.ActorRef
import akka.actor.Status.Success
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.{EncryBaseTransaction, EncryPaymentTransaction}
import encry.modifiers.state.box._
import encry.modifiers.state.TransactionValidator
import encry.modifiers.state.box.proposition.AddressProposition
import encry.settings.Algos
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.{EphemerealNodeViewModifier, VersionTag}
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

import scala.util.Try

class UtxoState(override val version: VersionTag,
                val store: Store,
                nodeViewHolderRef: Option[ActorRef])
  extends EncryBaseState[UtxoState] with TransactionValidator with ScorexLogging {

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


  // TODO: Why 10?
  override def maxRollbackDepth: Int = 10

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
    })
  }

  private[state] def checkTransactions(txs: Seq[EncryBaseTransaction]) = Try {
    // TODO: Достать выходы из бд и проверить соответствие адресов публ. ключам.
    txs.forall(tx => tx.semanticValidity.isSuccess)
  }

  // Dispatches applying `Modifier` of particular type.
  override def applyModifier(mod: EncryPersistentModifier): Try[UtxoState] = mod match {
    case block: EncryBlock =>
      log.debug(s"Applying block with header ${block.header.encodedId} to UtxoState with " +
        s"root hash ${Algos.encode(rootHash)}")

      if (checkTransactions(block.payload.transactions).get) {
        Try {
          // TODO: Пробуем применить изменения к `store`.
          new UtxoState(VersionTag @@ block.id, store, nodeViewHolderRef)
        }
      }
  }

  override def rollbackTo(version: VersionTag): Try[UtxoState] = ???

  override def rollbackVersions: Iterable[VersionTag] = ???

  override lazy val rootHash: ADDigest = ???

  override def validate(tx: EphemerealNodeViewModifier): Try[Unit] = ???

}

object UtxoState {

  private lazy val bestVersionKey = Algos.hash("best state version") // TODO: ???

  private def metadata(modId: VersionTag, stateRoot: ADDigest): Seq[(Array[Byte], Array[Byte])] = {
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modId -> stateRoot
    val stateDigestIdIdxElem = Algos.hash(stateRoot) -> modId
    val bestVersion = bestVersionKey -> modId

    Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion)
  }
}
