package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.account.{Account, Address}
import encry.common.KeyPairType
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.{AccountProposition, AccountPropositionSerializer}
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.{Failure, Success, Try}

case class PubKeyInfoBox(override val proposition: AccountProposition,
                         override val nonce: Long,
                         pubKeyBytes: PublicKey,
                         pubKeyProofBytes: Signature,
                         pubKeyInfoBytes: Array[Byte],
                         pubKeyTypeId: Byte)
  extends EncryBox[AccountProposition] {

  override type M = PubKeyInfoBox

  override val typeId: BxTypeId = PubKeyInfoBox.typeId

  override def unlockTry(modifier: EncryBaseTransaction,
                         script: Option[String] = None)(implicit ctxOpt: Option[Context] = None): Try[Unit] =
    if (modifier.accountPubKey.address != proposition.account.address) Failure(new Error("Unlock failed"))
    else Success()

  override def serializer: Serializer[M] = PubKeyInfoBoxSerializer

  override def json: Json = Map(
    "id" -> Algos.encode(id).asJson,
    "proposition" -> proposition.account.toString.asJson,
    "nonce" -> nonce.asJson,
    "publicKey" -> Algos.encode(pubKeyBytes).asJson,
    "publicKeyProof" -> Algos.encode(pubKeyProofBytes).asJson,
    "publicKeyInfo" -> Algos.encode(pubKeyInfoBytes).asJson,
    "publicKeyTypeName" -> KeyPairType.pairTypeById(pubKeyTypeId).name.asJson
  ).asJson
}

object PubKeyInfoBox {

  val typeId: BxTypeId = 4.toByte

  def apply(address: Address,
            nonce: Long,
            pubKeyBytes: PublicKey,
            pubKeyProofBytes: Signature,
            pubKeyInfoBytes: Array[Byte],
            pubKeyTypeId: Byte): PubKeyInfoBox =
    PubKeyInfoBox(AccountProposition(address), nonce, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId)

  def apply(account: Account,
            nonce: Long,
            pubKeyBytes: PublicKey,
            pubKeyProofBytes: Signature,
            pubKeyInfoBytes: Array[Byte],
            pubKeyTypeId: Byte): PubKeyInfoBox =
    PubKeyInfoBox(AccountProposition(account), nonce, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId)
}

object PubKeyInfoBoxSerializer extends Serializer[PubKeyInfoBox] {

  override def toBytes(obj: PubKeyInfoBox): Array[Byte] = {
    Bytes.concat(
      obj.proposition.bytes,
      Longs.toByteArray(obj.nonce),
      Ints.toByteArray(obj.pubKeyBytes.length),
      obj.pubKeyBytes,
      Ints.toByteArray(obj.pubKeyProofBytes.length),
      obj.pubKeyProofBytes,
      Ints.toByteArray(obj.pubKeyInfoBytes.length),
      obj.pubKeyInfoBytes,
      Array(obj.pubKeyTypeId)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[PubKeyInfoBox] = Try {
    val accountPropositionLen = AccountPropositionSerializer.Length
    val proposition = AccountPropositionSerializer.parseBytes(bytes.take(accountPropositionLen)).get
    val nonce = Longs.fromByteArray(bytes.slice(accountPropositionLen, accountPropositionLen + 8))
    val pubKeyDataStart = accountPropositionLen + 8
    val pubKeyBytesLen = Ints.fromByteArray(bytes.slice(pubKeyDataStart, pubKeyDataStart + 4))
    val pubKeyBytes = PublicKey @@ bytes.slice(pubKeyDataStart + 4, pubKeyDataStart + 4 + pubKeyBytesLen)
    val s1 = pubKeyDataStart + 4 + pubKeyBytesLen
    val pubKeyProofBytesLen = Ints.fromByteArray(bytes.slice(s1, s1 + 4))
    val pubKeyProofBytes = Signature @@ bytes.slice(s1 + 4, s1 + 4 + pubKeyProofBytesLen)
    val s2 = s1 + 4 + pubKeyProofBytesLen
    val pubKeyInfoBytesLen = Ints.fromByteArray(bytes.slice(s2, s2 + 4))
    val pubKeyInfoBytes = bytes.slice(s2 + 4, s2 + 4 + pubKeyInfoBytesLen)
    val pubKeyTypeId = bytes.last
    PubKeyInfoBox(proposition, nonce, pubKeyBytes, pubKeyProofBytes, pubKeyInfoBytes, pubKeyTypeId)
  }
}
