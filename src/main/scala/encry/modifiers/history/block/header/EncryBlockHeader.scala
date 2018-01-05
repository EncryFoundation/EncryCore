package encry.modifiers.history.block.header

import com.google.common.primitives.{Ints, _}
import encry.consensus.Difficulty
import encry.settings.{Algos, ChainSettings, Constants}
import encry.consensus.validation.PowConsensusValidator._
import encry.modifiers.ModifierWithDigest
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.payload.EncryBlockPayload
import io.circe.Json
import io.circe.syntax._
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

import scala.util.Try

// TODO: Add generator proposition + signature to the header to verify miner`s identity?
case class EncryBlockHeader(override val version: Version,
                            override val parentId: ModifierId,
                            override val adProofsRoot: Digest32,
                            override val stateRoot: ADDigest, // 32 bytes + 1 (tree height)
                            override val txsRoot: Digest32,
                            override val timestamp: Timestamp,
                            override val height: Int,
                            var nonce: Long = 0L,
                            difficulty: Difficulty) extends EncryBaseBlockHeader {

  override type M = EncryBlockHeader

  override val modifierTypeId: ModifierTypeId = EncryBlockHeader.modifierTypeId

  override lazy val id: ModifierId = ModifierId @@ hHash

  override lazy val headerBytes: Array[Byte] = {
    Bytes.concat(
      Array(version),
      parentId,
      adProofsRoot,
      stateRoot,
      txsRoot,
      Longs.toByteArray(timestamp),
      Ints.toByteArray(height),
      Longs.toByteArray(nonce),
      difficulty.toByteArray
    )
  }

  val hHash: Digest32 = Algos.hash(headerBytes)

  // Checks whether the block timestamp is less than
  // two hours in the future (7200000ms) (allowing for time errors).
  val validTimestamp: Boolean = (timestamp - System.currentTimeMillis()) < 7200000L

  lazy val isGenesis: Boolean = height == ChainSettings.genesisHeight

  lazy val payloadId: ModifierId =
    ModifierWithDigest.computeId(EncryBlockPayload.modifierTypeId, id, txsRoot)

  lazy val adProofsId: ModifierId = ModifierWithDigest.computeId(ADProofs.modifierTypeId, id, adProofsRoot)

  override def serializer: Serializer[M] = EncryBlockHeaderSerializer

  override lazy val json: Json = Map(
    "id" -> Algos.encode(id).asJson,
    "parentId" -> Algos.encode(payloadId).asJson,
    "stateRoot" -> Algos.encode(stateRoot).asJson,
    "txRoot" -> Algos.encode(txsRoot).asJson,
  ).asJson
}

object EncryBlockHeader {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (101: Byte)

  lazy val GenesisParentId: ModifierId = ModifierId @@ Array.fill(Constants.hashLength)(0: Byte)
}

object EncryBlockHeaderSerializer extends Serializer[EncryBlockHeader] {

  def toBytesWithoutPOW(obj: EncryBlockHeader): Array[Byte] = {
    Bytes.concat(
      Array(obj.version),
      obj.parentId,
      obj.adProofsId,
      obj.txsRoot,
      Longs.toByteArray(obj.timestamp),
      Ints.toByteArray(obj.height),
      obj.difficulty.toByteArray
    )
  }

  override def toBytes(obj: EncryBlockHeader): Array[Byte] = {
    Bytes.concat(
      toBytesWithoutPOW(obj),
      Longs.toByteArray(obj.nonce)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[EncryBlockHeader] = Try {
    val version = bytes.head
    val parentId = ModifierId @@ bytes.slice(1, 33)
    val adProofsRoot = Digest32 @@ bytes.slice(33, 65)
    val stateRoot =  ADDigest @@ bytes.slice(65, 98)  // 32 bytes + 1 (tree height)
    val txMerkleRoot = Digest32 @@ bytes.slice(98, 130)
    val timestamp = Longs.fromByteArray(bytes.slice(130, 138))
    val height = Ints.fromByteArray(bytes.slice(138, 142))
    val nonce = Longs.fromByteArray(bytes.slice(142, 150))
    val difficulty = Difficulty @@ BigInt.apply(bytes.slice(150, bytes.length-1))
    EncryBlockHeader(version, parentId, adProofsRoot, stateRoot, txMerkleRoot, timestamp, height, nonce, difficulty)
  }
}
