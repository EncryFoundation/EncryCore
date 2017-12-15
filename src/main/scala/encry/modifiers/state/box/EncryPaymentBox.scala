package encry.modifiers.state.box

import com.google.common.primitives.{Bytes, Longs}
import encry.crypto.Address
import encry.modifiers.state.box.body.PaymentBoxBody
import encry.modifiers.state.box.proposition.AddressProposition
import encry.modifiers.state.box.serializers.BoxCompanionSerializer
import io.circe.Json
import io.circe.syntax._
import scorex.core.ModifierId
import scorex.core.transaction.box.proposition.{Proposition, PublicKey25519Proposition}
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.util.Try

case class EncryPaymentBox(override val proposition: AddressProposition,
                           override val nonce: Long,
                           override val body: PaymentBoxBody)
  extends EncryNoncedBox[AddressProposition, PaymentBoxBody] {

  override type M = EncryPaymentBox

  override lazy val id: ADKey = ADKey @@ EncryPaymentBox.idFromBox(proposition, nonce)

  override def json: Json = ???

  override def serializer: BoxCompanionSerializer[EncryPaymentBox] = EncryPaymentBoxSerializer

}

object EncryPaymentBox {
  def idFromBox(addr: AddressProposition, nonce: Long): ModifierId =
    ModifierId @@ Blake2b256(addr.bytes ++ Longs.toByteArray(nonce))
}

object EncryPaymentBoxSerializer extends BoxCompanionSerializer[EncryPaymentBox] {

  val Length: Int = 8

  override def toBytes(obj: EncryPaymentBox): Array[Byte] = {
    Bytes.concat(
      AddressProposition.addrBytes(obj.proposition.address),
      Longs.toByteArray(obj.nonce),
      Longs.toByteArray(obj.body.amount)
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[EncryPaymentBox] = Try{
    val i = 37
    val proposition = new AddressProposition(Address @@ Base58.encode(bytes.slice(0,i)))
    val nonce = Longs.fromByteArray(bytes.slice(i,i+8))
    val boxBody = PaymentBoxBody(Longs.fromByteArray(bytes.slice(i+8,i+16)))
    EncryPaymentBox(proposition,nonce,boxBody)
  }
}
