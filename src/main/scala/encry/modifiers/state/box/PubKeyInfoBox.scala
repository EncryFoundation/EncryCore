package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs}
import encry.account.Address
import encry.modifiers.mempool.EncryTransaction
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.modifiers.state.box.proposition.AddressProposition
import encry.modifiers.state.box.serializers.SizedCompanionSerializer
import encry.settings.Algos
import io.circe.Json
import io.circe.syntax._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition.AddressLength
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.PublicKey

import scala.util.{Failure, Success, Try}

case class PubKeyInfoBox(override val proposition: AddressProposition,
                         override val nonce: Long,
                         pubKeyBytes: PublicKey,
                         pubKeyInfo: String)
  extends EncryNoncedBox[AddressProposition] {

  override type M = PubKeyInfoBox

  override val typeId: BxTypeId = PubKeyInfoBox.typeId

  override lazy val bxHash: Digest32 = Algos.hash(
    Bytes.concat(
      proposition.bytes,
      Longs.toByteArray(nonce),
      pubKeyBytes,
      pubKeyInfo.getBytes
    )
  )

  override def unlockTry(modifier: EncryTransaction, script: Option[String] = None,
                         ctxOpt: Option[Context] = None): Try[Unit] =
    if (modifier.proposition.address != proposition.address) Failure(new Error("Unlock failed"))
    else Success()

  override def serializer: SizedCompanionSerializer[M] = PubKeyInfoBoxSerializer

  override def json: Json = Map(
    "id" -> Algos.encode(id).asJson,
    "proposition" -> proposition.address.toString.asJson,
    "nonce" -> nonce.asJson,
    "publicKey" -> Algos.encode(pubKeyBytes).asJson,
    "publicKeyInfo" -> pubKeyInfo.asJson
  ).asJson
}

object PubKeyInfoBox {

  val typeId: BxTypeId = 3.toByte
}

object PubKeyInfoBoxSerializer extends SizedCompanionSerializer[PubKeyInfoBox] {

  val Size: Int = AddressLength + 40

  override def toBytes(obj: PubKeyInfoBox): Array[Byte] = {
    Bytes.concat(
      AddressProposition.getAddrBytes(obj.proposition.address),
      Longs.toByteArray(obj.nonce),
      obj.pubKeyBytes,
      obj.pubKeyInfo.getBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[PubKeyInfoBox] = Try {
    val proposition = new AddressProposition(Address @@ Algos.encode(bytes.slice(0, AddressLength)))
    val nonce = Longs.fromByteArray(bytes.slice(AddressLength, AddressLength + 8))
    val pubKeyDataStart = AddressLength + 8
    val pubKeyBytes = PublicKey @@ bytes.slice(pubKeyDataStart, pubKeyDataStart + 32)
    val pubKeyInfo = new String(bytes.slice(pubKeyDataStart + 32, bytes.length), "UTF-8")
    PubKeyInfoBox(proposition, nonce, pubKeyBytes, pubKeyInfo)
  }
}
