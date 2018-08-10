package encry.settings

import java.nio.charset.Charset
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleTree
import scorex.crypto.encode.Base16
import scorex.crypto.hash.{Blake2b256, Digest32}
import scala.util.Try

object Algos {

  type HF = Blake2b256.type

  def encode(bytes: Array[Byte]): String = encoder.encode(bytes)

  def decode(str: String): Try[Array[Byte]] = encoder.decode(str)

  val hash: HF = Blake2b256

  val encoder: Base16.type = Base16

  val charset: Charset = Charset.defaultCharset()

  def merkleTreeRoot(elements: Seq[LeafData]): Digest32 =
    if (elements.isEmpty) emptyMerkleTreeRoot else MerkleTree(elements)(hash).rootHash

  lazy val emptyMerkleTreeRoot: Digest32 = Algos.hash(LeafData @@ Array[Byte]())
}