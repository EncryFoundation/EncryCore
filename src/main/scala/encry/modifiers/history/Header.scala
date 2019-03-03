package encry.modifiers.history

import HeaderProto.HeaderProtoMessage
import cats.implicits._
import com.google.common.primitives.{Ints, _}
import com.google.protobuf.ByteString
import encry.consensus.ConsensusTaggedTypes.Difficulty
import encry.crypto.equihash.{Equihash, EquihashSolution, EquihashSolutionsSerializer}
import encry.modifiers.history.Block.{Height, Timestamp, Version}
import encry.modifiers.mempool.Transaction
import encry.modifiers.mempool.directive.TransferDirective
import encry.modifiers.{EncryPersistentModifier, ModifierWithDigest}
import encry.settings.Constants
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax._
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.encryfoundation.common.Algos
import org.encryfoundation.common.serialization.Serializer
import org.encryfoundation.common.utils.TaggedTypes.ADDigest
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest32

import scala.util.Try

case class Header(version: Version,
                  override val parentId: ModifierId,
                  adProofsRoot: Digest32,
                  stateRoot: ADDigest, // 32 bytes + 1 (tree height)
                  transactionsRoot: Digest32,
                  timestamp: Timestamp,
                  height: Height,
                  nonce: Long,
                  difficulty: Difficulty,
                  equihashSolution: EquihashSolution) extends EncryPersistentModifier {

  import Header._

  override type M = Header

  override val modifierTypeId: ModifierTypeId = Header.modifierTypeId

  def toHeaderProto: HeaderProtoMessage = HeaderProtoSerializer.toProto(this)

  lazy val powHash: Digest32 = getPowHash(this)

  lazy val requiredDifficulty: Difficulty = difficulty

  override lazy val id: ModifierId = ModifierId @@ powHash.untag(Digest32)

  lazy val isGenesis: Boolean = height == Constants.Chain.GenesisHeight

  lazy val payloadId: ModifierId =
    ModifierWithDigest.computeId(Payload.modifierTypeId, id, transactionsRoot)

  lazy val adProofsId: ModifierId = ModifierWithDigest.computeId(ADProofs.modifierTypeId, id, adProofsRoot)

  lazy val ADProofAndPayloadIds: Seq[ModifierId] = Seq(adProofsId, payloadId)

  def isRelated(mod: EncryPersistentModifier): Boolean = mod match {
    case p: ADProofs => adProofsRoot sameElements p.digest
    case t: Payload => transactionsRoot sameElements t.digest
    case _ => false
  }

  override def serializer: Serializer[M] = HeaderSerializer

  override def toString: String = s"Header(id=$encodedId, height=$height, parent=${Algos.encode(parentId)}, " +
    s"version = $version, adProofsRoot = ${Algos.encode(adProofsRoot)}, stateRoot = ${Algos.encode(stateRoot)}, " +
    s" transactionsRoot = ${Algos.encode(transactionsRoot)}, timestamp = $timestamp, nonce = $nonce, " +
    s"difficulty = $difficulty)"
}

case class HeaderDBVersion(id: String,
                           parentId: String,
                           version: Version,
                           height: Int,
                           proofsRoot: String,
                           stateRoot: String,
                           transactionsRoot: String,
                           ts: Long,
                           nonce: Long,
                           difficulty: Long,
                           length: Int,
                           solution: List[Int],
                           proofs: String,
                           txCount: Int,
                           minerAddress: String,
                           minerReward: Long,
                           feesTotal: Long,
                           txsSize: Int,
                           bestChain: Boolean,
                           adProofOpt: Option[String]) {
  def toHeader: Try[Header] = {
    (Base16.decode(parentId), Base16.decode(proofsRoot), Base16.decode(stateRoot), Base16.decode(transactionsRoot)).mapN {
      case (decodedParentId, decodedProofsRoot, decodedStateRoot, decodeTxRoot) =>
        Header(
          version,
          ModifierId @@ decodedParentId,
          Digest32 @@ decodedProofsRoot,
          ADDigest @@ decodedStateRoot,
          Digest32 @@ decodeTxRoot,
          ts,
          height,
          nonce,
          Difficulty @@ BigInt(difficulty),
          EquihashSolution(solution)
        )
    }
  }
}

object HeaderDBVersion {

  def apply(block: Block): HeaderDBVersion = {
    val (minerAddress: String, minerReward: Long) = minerInfo(block.payload.transactions.last)
    HeaderDBVersion(
      Base16.encode(block.header.id),
      Base16.encode(block.header.parentId),
      block.header.version,
      block.header.height,
      Base16.encode(block.header.adProofsRoot),
      Base16.encode(block.header.stateRoot),
      Base16.encode(block.header.transactionsRoot),
      block.header.timestamp,
      block.header.nonce,
      block.header.difficulty.toLong,
      block.bytes.length,
      block.header.equihashSolution.ints.toList,
      block.adProofsOpt.map(p => Base16.encode(p.bytes)).getOrElse(""),
      block.payload.transactions.size,
      minerAddress,
      minerReward,
      block.payload.transactions.map(_.fee).sum,
      block.payload.transactions.map(_.bytes.length).sum,
      bestChain = true,
      block.adProofsOpt.map(_.bytes).map(Base16.encode)
    )
  }

  def apply(header: Header): HeaderDBVersion = {
    HeaderDBVersion(
      Base16.encode(header.id),
      Base16.encode(header.parentId),
      header.version,
      header.height,
      Base16.encode(header.adProofsRoot),
      Base16.encode(header.stateRoot),
      Base16.encode(header.transactionsRoot),
      header.timestamp,
      header.nonce,
      header.difficulty.toLong,
      header.bytes.length,
      header.equihashSolution.ints.toList,
      "",
      0,
      "unknown",
      0L,
      0,
      0,
      bestChain = false,
      None
    )
  }

  private def minerInfo(coinbase: Transaction): (String, Long) = coinbase.directives.head match {
    case TransferDirective(address, amount, tokenIdOpt) if tokenIdOpt.isEmpty => address -> amount
    case _ => "unknown" -> 0
  }

}

object Header {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (101: Byte)

  lazy val GenesisParentId: ModifierId = ModifierId @@ Array.fill(Constants.DigestLength)(0: Byte)

  implicit val jsonEncoder: Encoder[Header] = (h: Header) => Map(
    "id"               -> Algos.encode(h.id).asJson,
    "version"          -> h.version.asJson,
    "parentId"         -> Algos.encode(h.parentId).asJson,
    "adProofsRoot"     -> Algos.encode(h.adProofsRoot).asJson,
    "payloadId"        -> Algos.encode(h.payloadId).asJson,
    "stateRoot"        -> Algos.encode(h.stateRoot).asJson,
    "txRoot"           -> Algos.encode(h.transactionsRoot).asJson,
    "nonce"            -> h.nonce.asJson,
    "timestamp"        -> h.timestamp.asJson,
    "height"           -> h.height.asJson,
    "difficulty"       -> h.difficulty.toString.asJson,
    "equihashSolution" -> h.equihashSolution.asJson
  ).asJson

  implicit val jsonDecoder: Decoder[Header] = (c: HCursor) => {
    for {
      version          <- c.downField("version").as[Byte]
      parentId         <- c.downField("parentId").as[String]
      adProofsRoot     <- c.downField("adProofsRoot").as[String]
      stateRoot        <- c.downField("stateRoot").as[String]
      txRoot           <- c.downField("txRoot").as[String]
      timestamp        <- c.downField("timestamp").as[Long]
      height           <- c.downField("height").as[Int]
      nonce            <- c.downField("nonce").as[Long]
      difficulty       <- c.downField("difficulty").as[BigInt]
      equihashSolution <- c.downField("equihashSolution").as[EquihashSolution]
    } yield Header(
      version,
      ModifierId @@ Algos.decode(parentId).getOrElse(Array.emptyByteArray),
      Digest32 @@ Algos.decode(adProofsRoot).getOrElse(Array.emptyByteArray),
      ADDigest @@ Algos.decode(stateRoot).getOrElse(Array.emptyByteArray),
      Digest32 @@ Algos.decode(txRoot).getOrElse(Array.emptyByteArray),
      timestamp,
      height,
      nonce,
      Difficulty @@ difficulty,
      equihashSolution
    )
  }

  def getPowHash(header: Header): Digest32 = {
    val digest: Blake2bDigest = new Blake2bDigest(256)
    val bytes: Array[Byte] = HeaderSerializer.bytesWithoutPow(header)
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

object HeaderProtoSerializer {

  //TODO check big int difficulty
  def toProto(header: Header): HeaderProtoMessage = HeaderProtoMessage()
    .withVersion(ByteString.copyFrom(Array(header.version)))
    .withParentId(ByteString.copyFrom(header.parentId))
    .withAdProofsRoot(ByteString.copyFrom(header.adProofsRoot))
    .withStateRoot(ByteString.copyFrom(header.stateRoot))
    .withTransactionsRoot(ByteString.copyFrom(header.transactionsRoot))
    .withTimestamp(header.timestamp)
    .withHeight(header.height)
    .withNonce(header.nonce)
    .withDifficulty(header.difficulty.toLong)
    .withEquihashSolution(EquihashSolution.toProto(header.equihashSolution))

  def fromProto(headerProtoMessage: HeaderProtoMessage): Try[Header] = Try {
    val eqs: EquihashSolution = headerProtoMessage.equihashSolution.map(m => EquihashSolution(m.ints)) match {
      case Some(value) => value
      case _ => throw new RuntimeException("No EquihashSolution in header!")
    }
    Header(
      headerProtoMessage.version.toByteArray.head,
      ModifierId @@ headerProtoMessage.parentId.toByteArray,
      Digest32 @@ headerProtoMessage.adProofsRoot.toByteArray,
      ADDigest @@ headerProtoMessage.stateRoot.toByteArray,
      Digest32 @@ headerProtoMessage.transactionsRoot.toByteArray,
      headerProtoMessage.timestamp,
      headerProtoMessage.height,
      headerProtoMessage.nonce,
      Difficulty @@ BigInt(headerProtoMessage.difficulty),
      eqs
    )
  }
}

object HeaderSerializer extends Serializer[Header] {

  def bytesWithoutPow(h: Header): Array[Byte] =
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

  override def toBytes(obj: Header): Array[Byte] =
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

  override def parseBytes(bytes: Array[Byte]): Try[Header] = Try {
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
    val equihashSolution: EquihashSolution =
      EquihashSolutionsSerializer.parseBytes(bytes.slice(154 + diificultySize, bytes.length)).get

    Header(version, parentId, adProofsRoot, stateRoot, txsRoot, timestamp, height, nonce, difficulty, equihashSolution)
  }
}
