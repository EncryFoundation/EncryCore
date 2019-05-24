package encry.modifiers.history

import encry.avltree.{BatchAVLVerifier, Insert, Modification, Remove}
import org.encryfoundation.common.modifiers.history.ADProofs
import org.encryfoundation.common.modifiers.state.box._
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.Algos.HF
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ADValue}
import scorex.crypto.hash.Digest32
import scala.util.{Failure, Success, Try}

object ADProofsUtils {

  val KeyLength: Int = 32

  /**
    * Verify a set of box(outputs) operations on authenticated UTXO set by using the proof (this class wraps).
    *
    * @param changes      - ordered sequence of box operations(remove/insert) to check against a tree with known
    * @param previousHash - hash(from previous block) to apply the proof to.
    * @param expectedHash - expected (declared by miner) hash. A hash after applying proof must be the same.
    * @return Success, if verification passed
    */
  def verify(adProof: ADProofs,
             changes: EncryBoxStateChanges,
             previousHash: ADDigest,
             expectedHash: ADDigest): Try[Unit] = {

    def applyChanges(verifier: BatchAVLVerifier[Digest32, Algos.HF],
                     changes: EncryBoxStateChanges): Try[Option[ADValue]] =
      changes.operations.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, o) =>
        t.flatMap(_ => verifier.performOneOperation(toModification(o)))
      }

    val verifier: BatchAVLVerifier[Digest32, HF] = new BatchAVLVerifier[Digest32, Algos.HF](
      previousHash, adProof.proofBytes, KeyLength, None, maxNumOperations = Some(changes.operations.size)
    )

    applyChanges(verifier, changes).flatMap(_ => verifier.digest match {
      case Some(digest) =>
        if (digest.sameElements(expectedHash)) Success()
        else Failure(new IllegalArgumentException(
          s"Unexpected result digest: ${Algos.encode(digest)} != ${Algos.encode(expectedHash)}"
        ))
      case None => Failure(new IllegalStateException("Digest is undefined"))
    })
  }

  /**
    * Convert operation over a box into an AVL+ tree modification
    *
    * @param op - operation over a box
    * @return AVL+ tree modification
    */

  def toModification(op: EncryBoxStateChangeOperation): Modification = op match {
    case Insertion(box) => box match {
      case bx: EncryBaseBox => Insert(bx.id, ADValue @@ bx.bytes)
      case _                => throw new Exception("Got state modifier of unknown type.")
    }
    case Removal(id) => Remove(id)
  }
}