package encry.modifiers.history

import com.google.common.primitives._
import encry.modifiers.EncryPersistentModifier
import encry.settings.Constants
import encry.mining.crypto.SimpleHash._
import encry.settings.Algos
import io.circe.Json
import org.bouncycastle.crypto.digests.SHA256Digest
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.crypto.hash.Digest32
import scorex.core.block.Block._
import scorex.crypto.encode.Base16

import scala.util.Try
import scala.util.control.Breaks._

class EncryBlockHeader(override val version: Version,
                       override val parentId: ModifierId,
                       override val txMerkleRoot: Digest32,
                       override val timestamp: Timestamp,
                       override val height: Int,
                       var nonce: Long = 0L,
                       val targetedDiff: Int) extends EncryBaseBlockHeader {

  override type M = EncryBlockHeader

  override val modifierTypeId: ModifierTypeId = EncryBlockHeader.modifierTypeId

  override lazy val id: ModifierId = ModifierId @@ powHash

  // Simple POW implementation.
  lazy val powHash: Digest32 = {
    val bytes = EncryBlockHeaderSerializer.toBytesWithoutPOW(this)
    def loop(): Digest32 = {
      val hb = Bytes.concat(bytes, Longs.toByteArray(nonce))
      val solution = Algos.hash(hb)
      if (Base16.encode(solution).slice(0, targetedDiff) == "0"*targetedDiff) {
        solution
      } else {
        nonce += 1
        loop()
      }
    }
    loop()
  }

  override def serializer: Serializer[M] = EncryBlockHeaderSerializer

  override def json: Json = ???
}

object EncryBlockHeader {

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
