package encry.modifiers.history

import com.google.common.primitives.Bytes
import encry.modifiers.state.box._
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest}
import encry.settings.{Algos, Constants}
import io.circe.Json
import io.circe.syntax._
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.crypto.authds.{ADDigest, ADValue, SerializedAdProof}
import scorex.crypto.authds.avltree.batch.{BatchAVLVerifier, Insert, Modification, Remove}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

import scala.util.{Failure, Success, Try}

case class ADProofs(headerId: ModifierId, proofBytes: SerializedAdProof)
  extends EncryPersistentModifier with ModifierWithDigest {

  override def digest: Digest32 = ADProofs.proofDigest(proofBytes)

  override val modifierTypeId: ModifierTypeId = ADProofs.modifierTypeId

  override type M = ADProofs

  override lazy val serializer: Serializer[ADProofs] = ADProofSerializer

  override lazy val json: Json = Map(
    "headerId" -> Base58.encode(headerId).asJson,
    "proofBytes" -> Base58.encode(proofBytes).asJson,
    "digest" -> Base58.encode(digest).asJson
  ).asJson

  override def toString: String = s"ADProofs(${Base58.encode(id)},${Base58.encode(headerId)},${Base58.encode(proofBytes)})"

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

    def applyChanges(verifier: BatchAVLVerifier[Digest32, Blake2b256Unsafe],
                     changes: EncryBoxStateChanges) =
      changes.operations.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, o) =>
        t.flatMap(_ => {
          verifier.performOneOperation(ADProofs.toModification(o))
        })
      }

    val verifier = new BatchAVLVerifier[Digest32, Blake2b256Unsafe](previousHash, proofBytes, ADProofs.KL,
      None, maxNumOperations = Some(changes.operations.size))

    applyChanges(verifier, changes).flatMap { _ =>
      verifier.digest match {
        case Some(digest) =>
          if (digest sameElements expectedHash) {
            Success()
          } else {
            Failure(new IllegalArgumentException(s"Unexpected result digest: ${Base58.encode(digest)} != ${Base58.encode(expectedHash)}"))
          }
        case None =>
          Failure(new IllegalStateException("Digest is undefined"))
      }
    }
  }
}

object ADProofs {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (104: Byte)

  // TODO: WARN
  val KL = 32

  def proofDigest(proofBytes: SerializedAdProof): Digest32 = Algos.hash(proofBytes)

  /**
    * Convert operation over a box into an AVL+ tree modification
    * @param op - operation over a box
    * @return AVL+ tree modification
    */
  def toModification(op: EncryBoxStateChangeOperation): Modification =
    op match {
      case Insertion(box) => box match {
        case bx: AssetBox => Insert(bx.id, ADValue @@ bx.bytes)
        case bx: OpenBox => Insert(bx.id, ADValue @@ bx.bytes)
        case _ => throw new Error("Got state modifier of unknown type.")
      }
      case r: Removal => Remove(r.boxId)
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
