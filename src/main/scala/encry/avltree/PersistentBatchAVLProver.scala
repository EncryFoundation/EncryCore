package encry.avltree

import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ADKey, ADValue, SerializedAdProof}
import scorex.crypto.hash._
import scala.util.Try

trait PersistentBatchAVLProver[D <: Digest, HF <: CryptographicHash[D]] {

  var avlProver: BatchAVLProver[D, HF]
  val storage: VersionedIODBAVLStorage[D]

  def digest: ADDigest = avlProver.digest

  def unauthenticatedLookup(key: ADKey): Option[ADValue] = avlProver.unauthenticatedLookup(key)

  def performOneOperation(operation: Operation): Try[Option[ADValue]] = avlProver.performOneOperation(operation)

  def generateProofAndUpdateStorage[K <: Array[Byte], V <: Array[Byte]](additionalData: Seq[(K, V)]): SerializedAdProof = {
    storage.update(avlProver, additionalData).get
    avlProver.generateProof()
  }

  def generateProofAndUpdateStorage(): SerializedAdProof = generateProofAndUpdateStorage(Seq())

  def rollback(version: ADDigest): Try[Unit] = Try {
    val recoveredTop: (EncryProverNodes[D], Int) = storage.rollback(version).get
    avlProver = new BatchAVLProver(avlProver.keyLength, avlProver.valueLengthOpt, Some(recoveredTop))(avlProver.hf)
  }

  def checkTree(postProof: Boolean = false): Unit = avlProver.checkTree(postProof)
}

object PersistentBatchAVLProver {
  def create[D <: Digest,
  HF <: CryptographicHash[D],
  K <: Array[Byte],
  V <: Array[Byte]](
                     avlBatchProver: BatchAVLProver[D, HF],
                     versionedStorage: VersionedIODBAVLStorage[D],
                     additionalData: Seq[(K, V)],
                     paranoidChecks: Boolean
                   ): Try[PersistentBatchAVLProver[D, HF]] = Try {

    new PersistentBatchAVLProver[D, HF] {
      override var avlProver: BatchAVLProver[D, HF] = avlBatchProver
      override val storage: VersionedIODBAVLStorage[D] = versionedStorage

      (storage.version match {
        case Some(ver) => rollback(ver).get
        case None => generateProofAndUpdateStorage(additionalData) //to initialize storage and clear prover's state
      }).ensuring { _ =>
        storage.version.get.sameElements(avlProver.digest) &&
          (!paranoidChecks || Try(avlProver.checkTree(true)).isSuccess)
      }
    }
  }

  def create[D <: Digest, HF <: CryptographicHash[D]](
                                                       avlBatchProver: BatchAVLProver[D, HF],
                                                       versionedStorage: VersionedIODBAVLStorage[D],
                                                       paranoidChecks: Boolean = false
                                                     ): Try[PersistentBatchAVLProver[D, HF]] =
    create(avlBatchProver, versionedStorage, Seq(), paranoidChecks)
}