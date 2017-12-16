package encry.modifiers.history.block.header

import com.google.common.primitives._
import encry.settings.{Algos, Constants}
import encry.mining.crypto.SimpleHash
import io.circe.Json
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32

import scala.util.Try

case class EncryBlockHeader(override val version: Version,
                            override val parentId: ModifierId,
                            override val txMerkleRoot: Digest32,
                            override val timestamp: Timestamp,
                            override val height: Int,
                            var nonce: Long = 0L,
                            difficulty: Int,
                            generatorProposition: PublicKey25519Proposition) extends EncryBaseBlockHeader {

  import encry.modifiers.history.block.header.EncryBlockHeader._

  override type M = EncryBlockHeader

  override val modifierTypeId: ModifierTypeId = EncryBlockHeader.modifierTypeId

  override lazy val id: ModifierId = ModifierId @@ Algos.hash(headerBytes)

  override lazy val headerBytes: Array[Byte] = {
    Bytes.concat(
      Array(version),
      parentId,
      txMerkleRoot,
      Longs.toByteArray(timestamp),
      Ints.toByteArray(height),
      Longs.toByteArray(nonce),
      Ints.toByteArray(difficulty),
      generatorProposition.pubKeyBytes
    )
  }

  val validPow: Boolean = validatePow(id, difficulty)

  // Simple POW implementation.
//  lazy val powHash: Digest32 = {
//    val headerBytes = EncryBlockHeaderSerializer.toBytesWithoutPOW(this)
//    def loop(): Digest32 = {
//      val noncedHeaderBytes = Bytes.concat(headerBytes, Longs.toByteArray(nonce))
//      val solution = Algos.hash(noncedHeaderBytes)
//      if (SimpleHash.validateSolution(solution, difficultyBits)) {
//        println("Solution is: " + Base16.encode(solution))
//        solution
//      } else {
//        println("> " + Base16.encode(solution))
//        nonce += 1
//        loop()
//      }
//    }
//    loop()
//  }

  override def serializer: Serializer[M] = EncryBlockHeaderSerializer

  override def json: Json = ???
}

object EncryBlockHeader {

  def validatePow(headerHash: Array[Byte], difficulty: BigInt): Boolean = {
    assert(difficulty > 0, "Difficulty coefficient can not be less than 1")
    val maxTarget = BigInt(1, Array.fill(32)(Byte.MinValue))
    val target = maxTarget / difficulty
    println(s"Current target is: $target")
    BigInt(1, headerHash) < target
  }

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (101: Byte)

  lazy val GenesisParentId: ModifierId = ModifierId @@ Array.fill(Constants.hashLength)(0: Byte)
}

object EncryBlockHeaderSerializer extends Serializer[EncryBlockHeader] {

  def toBytesWithoutPOW(obj: EncryBaseBlockHeader): Array[Byte] = {
    Bytes.concat(
      Array(obj.version),
      obj.parentId,
      obj.txMerkleRoot,
      Longs.toByteArray(obj.timestamp),
      Ints.toByteArray(obj.height)
    )
  }

  override def toBytes(obj: EncryBlockHeader): Array[Byte] = {
    Bytes.concat(
      toBytesWithoutPOW(obj),
      Longs.toByteArray(obj.nonce)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[EncryBlockHeader] = ???
}
