package encry.modifiers.mempool.directive

import com.google.common.primitives.{Bytes, Ints}
import encry.account.{Account, Address}
import encry.common.KeyPairType
import encry.modifiers.mempool.directive.Directive.DirTypeId
import encry.modifiers.state.box.{EncryBaseBox, PubKeyInfoBox}
import encry.settings.Algos
import encry.utils.Utils
import io.circe.{Encoder, Json}
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.Try

case class AddPubKeyInfoDirective(address: Address,
                                  pubKeyBytes: PublicKey,
                                  pubKeyProofBytes: Signature,
                                  pubKeyInfoBytes: Array[Byte],
                                  pubKeyTypeId: Byte,
                                  override val idx: Int) extends Directive {

  override type M = AddPubKeyInfoDirective

  override val typeId: DirTypeId = AddPubKeyInfoDirective.TypeId

  override def boxes(digest: Digest32): Seq[EncryBaseBox] =
    Seq(PubKeyInfoBox(address, Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx)),
      pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId))

  override val cost: Amount = 10

  override val isValid: Boolean = pubKeyBytes.length >= 20 && pubKeyBytes.length <= 128

  override def serializer: Serializer[M] = AddPubKeyInfoDirectiveSerializer
}

object AddPubKeyInfoDirective {

  val TypeId: DirTypeId = 2.toByte

  implicit val jsonEncoder: Encoder[AddPubKeyInfoDirective] = (d: AddPubKeyInfoDirective) => Map(
    "typeId" -> d.typeId.asJson,
    "verboseType" -> "ADD_PUBKEY_INFO".asJson,
    "publicKey" -> Algos.encode(d.pubKeyBytes).asJson,
    "publicKeyProof" -> Algos.encode(d.pubKeyProofBytes).asJson,
    "publicKeyInfo" -> Algos.encode(d.pubKeyInfoBytes).asJson,
    "publicKeyTypeName" -> KeyPairType.pairTypeById(d.pubKeyTypeId).name.asJson,
    "idx" -> d.idx.asJson
  ).asJson
}

object AddPubKeyInfoDirectiveSerializer extends Serializer[AddPubKeyInfoDirective] {

  override def toBytes(obj: AddPubKeyInfoDirective): Array[DirTypeId] = Bytes.concat(
    Account.decodeAddress(obj.address),
    Ints.toByteArray(obj.pubKeyBytes.length),
    obj.pubKeyBytes,
    Ints.toByteArray(obj.pubKeyProofBytes.length),
    obj.pubKeyProofBytes,
    Ints.toByteArray(obj.pubKeyInfoBytes.length),
    obj.pubKeyInfoBytes,
    Array(obj.pubKeyTypeId),
    Ints.toByteArray(obj.idx)
  )

  override def parseBytes(bytes: Array[DirTypeId]): Try[AddPubKeyInfoDirective] = Try {
    val address = Address @@ Base58.encode(bytes.take(Account.AddressLength))
    val pubKeyDataStart = Account.AddressLength
    val pubKeyBytesLen = Ints.fromByteArray(bytes.slice(pubKeyDataStart, pubKeyDataStart + 4))
    val pubKeyBytes = PublicKey @@ bytes.slice(pubKeyDataStart + 4, pubKeyDataStart + 4 + pubKeyBytesLen)
    val s1 = pubKeyDataStart + 4 + pubKeyBytesLen
    val pubKeyProofBytesLen = Ints.fromByteArray(bytes.slice(s1, s1 + 4))
    val pubKeyProofBytes = Signature @@ bytes.slice(s1 + 4, s1 + 4 + pubKeyProofBytesLen)
    val s2 = s1 + 4 + pubKeyProofBytesLen
    val pubKeyInfoBytesLen = Ints.fromByteArray(bytes.slice(s2, s2 + 4))
    val pubKeyInfoBytes = bytes.slice(s2 + 4, s2 + 4 + pubKeyInfoBytesLen)
    val pubKeyTypeId = bytes.takeRight(5).head
    val idx = Ints.fromByteArray(bytes.takeRight(4))
    AddPubKeyInfoDirective(address, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId, idx)
  }
}
