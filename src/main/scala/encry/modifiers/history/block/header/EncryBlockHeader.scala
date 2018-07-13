package encry.modifiers.history.block.header

import com.google.common.primitives.{Ints, _}
import encry.consensus.Difficulty
import encry.crypto.equihash.{Equihash, EquihashSolution, EquihashSolutionsSerializer}
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.Block._
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.serialization.Serializer
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest}
import encry.settings.{Algos, Constants}
import encry.{ModifierId, ModifierTypeId}
import io.circe.Encoder
import io.circe.syntax._
import org.bouncycastle.crypto.digests.Blake2bDigest
import scorex.crypto.authds.ADDigest
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32

import scala.util.Try

case class EncryBlockHeader(override val version: Version,
                            override val parentId: ModifierId,
                            override val adProofsRoot: Digest32,
                            override val stateRoot: ADDigest, // 32 bytes + 1 (tree height)
                            override val transactionsRoot: Digest32,
                            override val timestamp: Timestamp,
                            override val height: Height,
                            nonce: Long,
                            difficulty: Difficulty,
                            equihashSolution: EquihashSolution) extends EncryBaseBlockHeader {

  import EncryBlockHeader._

  override type M = EncryBlockHeader

  override val modifierTypeId: ModifierTypeId = EncryBlockHeader.modifierTypeId

  lazy val powHash: Digest32 = getPowHash(this)

  lazy val requiredDifficulty: Difficulty = difficulty

  override lazy val id: ModifierId = ModifierId @@ powHash.untag(Digest32)

  lazy val isGenesis: Boolean = height == Constants.Chain.GenesisHeight

  lazy val payloadId: ModifierId =
    ModifierWithDigest.computeId(EncryBlockPayload.modifierTypeId, id, transactionsRoot)

  lazy val adProofsId: ModifierId = ModifierWithDigest.computeId(ADProofs.modifierTypeId, id, adProofsRoot)

  lazy val partsIds: Seq[ModifierId] = Seq(adProofsId, payloadId)

  def isRelated(mod: EncryPersistentModifier): Boolean = mod match {
    case p: ADProofs => adProofsRoot sameElements p.digest
    case t: EncryBlockPayload => transactionsRoot sameElements t.digest
    case _ => false
  }

  override def serializer: Serializer[M] = EncryBlockHeaderSerializer

  override def toString: String = s"Header(id=$encodedId, height=$height)"
}

object EncryBlockHeader {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (101: Byte)

  lazy val GenesisParentId: ModifierId = ModifierId @@ Array.fill(Constants.DigestLength)(0: Byte)

  implicit val jsonEncoder: Encoder[EncryBlockHeader] = (h: EncryBlockHeader) => Map(
    "id" -> Algos.encode(h.id).asJson,
    "hash" -> Base16.encode(h.id).asJson,
    "parentId" -> Algos.encode(h.parentId).asJson,
    "payloadId" -> Algos.encode(h.payloadId).asJson,
    "stateRoot" -> Algos.encode(h.stateRoot).asJson,
    "txRoot" -> Algos.encode(h.transactionsRoot).asJson,
    "timestamp" -> h.timestamp.asJson,
    "height" -> h.height.asJson,
    "difficulty" -> h.difficulty.toString.asJson,
  ).asJson

  def getPowHash(header: EncryBlockHeader): Digest32 = {
    val digest: Blake2bDigest = new Blake2bDigest(256)
    val bytes: Array[Byte] = EncryBlockHeaderSerializer.bytesWithoutPow(header)
    digest.update(bytes, 0, bytes.length)
    Equihash.hashNonce(digest, header.nonce)
    Equihash.hashSolution(digest, header.equihashSolution)
    val h: Array[Byte] = new Array[Byte](32)
    digest.doFinal(h, 0)

    val secondDigest: Blake2bDigest = new Blake2bDigest(256)
    secondDigest.update(h, 0, h.length)
    val result: Array[Byte] = new Array[Byte](32)
    secondDigest.doFinal(result, 0)

    Digest32 @@ result
  }
}

object EncryBlockHeaderSerializer extends Serializer[EncryBlockHeader] {

  def bytesWithoutPow(h: EncryBlockHeader): Array[Byte] =
    Bytes.concat(
      Array(h.version),
      h.parentId,
      h.adProofsRoot,
      h.transactionsRoot,
      h.stateRoot,
      Longs.toByteArray(h.timestamp),
      Ints.toByteArray(h.difficulty.toByteArray.length),
      h.difficulty.toByteArray,
      Ints.toByteArray(h.height))

  override def toBytes(obj: EncryBlockHeader): Array[Byte] =
    Bytes.concat(
      Array(obj.version),
      obj.parentId,
      obj.adProofsRoot,
      obj.stateRoot,
      obj.transactionsRoot,
      Longs.toByteArray(obj.timestamp),
      Ints.toByteArray(obj.height),
      Longs.toByteArray(obj.nonce),
      Ints.toByteArray(obj.difficulty.toByteArray.length),
      obj.difficulty.toByteArray,
      obj.equihashSolution.bytes
    )

  override def parseBytes(bytes: Array[Byte]): Try[EncryBlockHeader] = Try {
    val version: Version = bytes.head
    val parentId: ModifierId = ModifierId @@ bytes.slice(1, 33)
    val adProofsRoot: Digest32 = Digest32 @@ bytes.slice(33, 65)
    val stateRoot: ADDigest = ADDigest @@ bytes.slice(65, 98) // 32 bytes + 1 (tree height)
    val txsRoot: Digest32 = Digest32 @@ bytes.slice(98, 130)
    val timestamp: Long = Longs.fromByteArray(bytes.slice(130, 138))
    val height: Int = Ints.fromByteArray(bytes.slice(138, 142))
    val nonce: Long = Longs.fromByteArray(bytes.slice(142, 150))
    val diificultySize: Int = Ints.fromByteArray(bytes.slice(150, 154))
    val difficulty: Difficulty = Difficulty @@ BigInt(bytes.slice(154, 154 + diificultySize))
    val equihashSolution: EquihashSolution = EquihashSolutionsSerializer.parseBytes(bytes.slice(154 + diificultySize, bytes.length)).get

    EncryBlockHeader(version, parentId, adProofsRoot, stateRoot, txsRoot, timestamp, height, nonce, difficulty, equihashSolution)
  }
}
