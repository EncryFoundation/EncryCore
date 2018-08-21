package scorex.crypto.authds.avltree.batch

import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest

import scala.util.Try

trait VersionedAVLStorage[D <: Digest] {

  def update[K <: Array[Byte], V <: Array[Byte]](batchProver: BatchAVLProver[D, _],
                                                 additionalData: Seq[(K, V)]): Try[Unit]

  def rollback(version: ADDigest): Try[(ProverNodes[D], Int)]

  def version: Option[ADDigest]

  def isEmpty: Boolean = version.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def rollbackVersions: Iterable[ADDigest]
}
