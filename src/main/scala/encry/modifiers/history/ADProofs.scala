package encry.modifiers.history

import com.google.common.primitives.Bytes
import encry.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.avltree.{BatchAVLVerifier, Insert, Modification, Remove}
import encry.modifiers.state.box._
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest}
import encry.settings.Constants
import io.circe.Encoder
import io.circe.syntax._
import org.encryfoundation.common.Algos
import org.encryfoundation.common.serialization.Serializer
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ADValue, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scala.util.{Failure, Success, Try}

case class ADProofs(headerId: ModifierId, proofBytes: SerializedAdProof)
  extends EncryPersistentModifier with ModifierWithDigest {

  override def digest: Digest32 = ADProofs.proofDigest(proofBytes)

  override val modifierTypeId: ModifierTypeId = ADProofs.modifierTypeId

  override type M = ADProofs

  override lazy val serializer: Serializer[ADProofs] = ADProofSerializer

  override def toString: String = s"ADProofs(${Algos.encode(id)},${Algos.encode(headerId)},${Algos.encode(proofBytes)})"

  /**
    * Verify a set of box(outputs) operations on authenticated UTXO set by using the proof (this class wraps).
    *
    * @param changes      - ordered sequence of box operations(remove/insert) to check against a tree with known
    * @param previousHash - hash(from previous block) to apply the proof to.
    * @param expectedHash - expected (declared by miner) hash. A hash after applying proof must be the same.
    * @return Success, if verification passed
    */
  def verify(changes: EncryBoxStateChanges,
             previousHash: ADDigest,
             expectedHash: ADDigest): Try[Unit] = {

    def applyChanges(verifier: BatchAVLVerifier[Digest32, Algos.HF],
                     changes: EncryBoxStateChanges): Try[Option[ADValue]] =
      changes.operations.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, o) =>
        t.flatMap(_ => {
          verifier.performOneOperation(ADProofs.toModification(o))
        })
      }

    val verifier = new BatchAVLVerifier[Digest32, Algos.HF](previousHash, proofBytes, ADProofs.KeyLength,
      None, maxNumOperations = Some(changes.operations.size))

    applyChanges(verifier, changes).flatMap { _ =>
      verifier.digest match {
        case Some(digest) =>
          if (digest sameElements expectedHash) {
            Success()
          } else {
            Failure(new IllegalArgumentException(s"Unexpected result digest: ${Algos.encode(digest)} != ${Algos.encode(expectedHash)}"))
          }
        case None =>
          Failure(new IllegalStateException("Digest is undefined"))
      }
    }
  }
}

object ADProofs {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (104: Byte)

  val KeyLength = 32

  implicit val jsonEncoder: Encoder[ADProofs] = (p: ADProofs) => Map(
    "headerId" -> Algos.encode(p.headerId).asJson,
    "proofBytes" -> Algos.encode(p.proofBytes).asJson,
    "digest" -> Algos.encode(p.digest).asJson
  ).asJson

  def proofDigest(proofBytes: SerializedAdProof): Digest32 = Algos.hash(proofBytes)

  /**
    * Convert operation over a box into an AVL+ tree modification
    * @param op - operation over a box
    * @return AVL+ tree modification
    */
  def toModification(op: EncryBoxStateChangeOperation): Modification =
    op match {
      case Insertion(box) => box match {
        case bx: EncryBaseBox => Insert(bx.id, ADValue @@ bx.bytes)
        case _ => throw new Exception("Got state modifier of unknown type.")
      }
      case Removal(id) => Remove(id)
    }
}

object ADProofSerializer extends Serializer[ADProofs] {

  override def toBytes(obj: ADProofs): Array[Byte] = Bytes.concat(obj.headerId, obj.proofBytes)

  override def parseBytes(bytes: Array[Byte]): Try[ADProofs] = Try {
    ADProofs(
      ModifierId @@ bytes.take(Constants.ModifierIdSize),
      SerializedAdProof @@ bytes.slice(Constants.ModifierIdSize, bytes.length))
  }
}
