package encry.view.state

import akka.actor.ActorRef
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.{EncryBaseTransaction, EncryPaymentTransaction}
import encry.modifiers.state.box.{EncryBaseBox, EncryPaymentBox, EncryPaymentBoxSerializer}
import encry.modifiers.state.box.body.{BaseBoxBody, PaymentBoxBody}
import encry.settings.Algos
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.VersionTag
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.TransactionValidation
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}
import scorex.core.transaction.box.proposition.Proposition

import scala.util.Try

class UtxoState(override val version: VersionTag,
                val store: Store,
                nodeViewHolderRef: Option[ActorRef])
  extends EncryBaseState[Proposition, BaseBoxBody, EncryBaseBox[_, _], EncryBaseTransaction[_, _, _], UtxoState]
    with TransactionValidation[PublicKey25519Proposition, EncryPaymentTransaction] with ScorexLogging {

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

  // Dispatches applying `Modifier` of particular type.
  override def applyModifier(mod: EncryPersistentModifier): Try[UtxoState] = mod match {
    case pb: EncryBlock =>
      log.debug(s"Applying block with header ${pb.header.encodedId} to UtxoState with " +
        s"root hash ${Algos.encode(rootHash)}")

  }

  override def rollbackTo(version: VersionTag): Try[UtxoState] = ???

  override def rollbackVersions: Iterable[VersionTag] = ???

  override lazy val rootHash: ADDigest = ???

  override def validate(tx: EncryPaymentTransaction): Try[Unit] = ???

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
