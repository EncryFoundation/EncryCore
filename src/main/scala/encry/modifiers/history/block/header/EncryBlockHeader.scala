package encry.modifiers.history.block.header

import com.google.common.primitives.{Ints, _}
import encry.consensus.{Difficulty, DifficultySerializer, NBits}
import encry.crypto.PublicKey25519
import encry.crypto.equihash.{Equihash, EquihashSolution, EquihashSolutionsSerializer}
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.state.box.proof.Signature25519
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest}
import encry.settings.{Algos, Constants}
import io.circe.Encoder
import io.circe.syntax._
import org.bouncycastle.crypto.digests.Blake2bDigest
import scorex.core.block.Block._
import scorex.core.serialization.Serializer
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.crypto.authds.ADDigest
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.Try

case class EncryBlockHeader(override val version: Version,
                             override val accountPubKey: PublicKey25519,
                             override val signature: Signature25519,
                             override val parentId: ModifierId,
                             override val adProofsRoot: Digest32,
                             override val stateRoot: ADDigest, // 32 bytes + 1 (tree height)
                             override val transactionsRoot: Digest32,
                             override val timestamp: Timestamp,
                             override val height: Height,
                             var nonce: Long = 0L,
                             nBits: NBits,
                             equihashSolution: EquihashSolution) extends EncryBaseBlockHeader {

  import EncryBlockHeader._

  override type M = EncryBlockHeader

  override val modifierTypeId: ModifierTypeId = EncryBlockHeader.modifierTypeId

  lazy val powHash: Digest32 = getPowHash(this)

  lazy val requiredDifficulty: Difficulty = Difficulty @@ DifficultySerializer.decodeCompactBits(nBits)

  override lazy val id: ModifierId = ModifierId @@ powHash.untag(Digest32)

  override val dataToSign: Array[Byte] =
    getMessageToSign(version, accountPubKey, parentId, adProofsRoot, stateRoot, transactionsRoot, timestamp, height, nBits)

  lazy val isGenesis: Boolean = height == Constants.Chain.GenesisHeight

  lazy val payloadId: ModifierId =
    ModifierWithDigest.computeId(EncryBlockPayload.modifierTypeId, id, transactionsRoot)

  lazy val adProofsId: ModifierId = ModifierWithDigest.computeId(ADProofs.modifierTypeId, id, adProofsRoot)

  def isRelated(mod: EncryPersistentModifier): Boolean = mod match {
    case p: ADProofs => adProofsRoot sameElements p.digest
    case t: EncryBlockPayload => transactionsRoot sameElements t.digest
    case _ => false
  }

  override def serializer: Serializer[M] = EncryBlockHeaderSerializer
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
    "nBits" -> h.nBits.untag(NBits).asJson,
  ).asJson

  def getPowHash(header: EncryBlockHeader): Digest32 = {
    val digest = new Blake2bDigest(256)
    val bytes = EncryBlockHeaderSerializer.bytesWithoutPow(header)
    digest.update(bytes, 0, bytes.length)
    Equihash.hashNonce(digest, header.nonce)
    Equihash.hashSolution(digest, header.equihashSolution)
    val h = new Array[Byte](32)
    digest.doFinal(h, 0)

    val secondDigest = new Blake2bDigest(256)
    secondDigest.update(h, 0, h.length)
    val result = new Array[Byte](32)
    secondDigest.doFinal(result, 0)

    Digest32 @@ result
  }

  def getMessageToSign(version: Version,
                       accountPubKey: PublicKey25519,
                       parentId: ModifierId,
                       adProofsRoot: Digest32,
                       stateRoot: ADDigest, // 32 bytes + 1 (tree height)
                       txsRoot: Digest32,
                       timestamp: Timestamp,
                       height: Int,
                       nBits: NBits): Array[Byte] = Algos.hash(
    Bytes.concat(
      Array(version),
      accountPubKey.pubKeyBytes,
      parentId,
      adProofsRoot,
      stateRoot,
      txsRoot,
      Longs.toByteArray(timestamp),
      Ints.toByteArray(height),
      Longs.toByteArray(nBits)
    )
  )
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
      DifficultySerializer.uint32ToByteArrayBE(h.nBits),
      Ints.toByteArray(h.height))

  override def toBytes(obj: EncryBlockHeader): Array[Byte] =
    Bytes.concat(
      Array(obj.version),
      obj.accountPubKey.pubKeyBytes,
      obj.signature.signature,
      obj.parentId,
      obj.adProofsRoot,
      obj.stateRoot,
      obj.transactionsRoot,
      Longs.toByteArray(obj.timestamp),
      Ints.toByteArray(obj.height),
      Longs.toByteArray(obj.nonce),
      DifficultySerializer.uint32ToByteArrayBE(obj.nBits),
      obj.equihashSolution.bytes
    )

  override def parseBytes(bytes: Array[Byte]): Try[EncryBlockHeader] = Try {
    val version = bytes.head
    val proposition = PublicKey25519(PublicKey @@ bytes.slice(1, 33))
    val signature = Signature25519(Signature @@ bytes.slice(33, 97))
    val parentId = ModifierId @@ bytes.slice(97, 129)
    val adProofsRoot = Digest32 @@ bytes.slice(129, 161)
    val stateRoot =  ADDigest @@ bytes.slice(161, 194)  // 32 bytes + 1 (tree height)
    val txsRoot = Digest32 @@ bytes.slice(194, 226)
    val timestamp = Longs.fromByteArray(bytes.slice(226, 234))
    val height = Ints.fromByteArray(bytes.slice(234, 238))
    val nonce = Longs.fromByteArray(bytes.slice(238, 246))
    val difficulty = DifficultySerializer.readUint32BE(bytes.slice(246, 250))
    val equihashSolution = EquihashSolutionsSerializer.parseBytes(bytes.slice(250, bytes.length)).get

    EncryBlockHeader(
      version, proposition, signature, parentId, adProofsRoot, stateRoot, txsRoot, timestamp, height, nonce, difficulty, equihashSolution)
  }
}
