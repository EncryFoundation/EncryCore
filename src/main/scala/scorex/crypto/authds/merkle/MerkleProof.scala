package scorex.crypto.authds.merkle

import scorex.crypto.authds.{LeafData, Side}
import scorex.crypto.hash.{CryptographicHash, Digest}
import scorex.utils.ScorexEncoding

case class MerkleProof[D <: Digest](leafData: LeafData, levels: Seq[(Digest, Side)])
                                   (implicit val hf: CryptographicHash[D]) extends ScorexEncoding {

  def valid(expectedRootHash: Digest): Boolean = {
    val leafHash = hf.prefixedHash(MerkleTree.LeafPrefix, leafData)

    levels.foldLeft(leafHash) { case (prevHash, (hash, side)) =>
      if (side == MerkleProof.LeftSide) {
        hf.prefixedHash(MerkleTree.InternalNodePrefix, prevHash ++ hash)
      } else {
        hf.prefixedHash(MerkleTree.InternalNodePrefix, hash ++ prevHash)
      }
    }.sameElements(expectedRootHash)
  }

  override def toString: String =
    s"MerkleProof(data: ${encoder.encode(leafData)}, hash: ${encoder.encode(hf(leafData))}, " +
      s"(${levels.map(ht => encoder.encode(ht._1) + " : " + ht._2)}))"
}

object MerkleProof {

  val LeftSide: Side = Side @@ 0.toByte
  val RightSide: Side = Side @@ 1.toByte
}

