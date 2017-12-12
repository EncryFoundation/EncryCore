package encry.modifiers.state.box

import encry.modifiers.state.box.body.PaymentBoxBody
import encry.modifiers.state.box.serializers.BoxCompanionSerializer
import io.circe.Json
import io.circe.syntax._
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.util.Try

case class EncryPaymentBox(override val proposition: PublicKey25519Proposition,
                           override val nonce: Long,
                           override val body: PaymentBoxBody)
  extends EncryPublicKeyNoncedBox[PublicKey25519Proposition, PaymentBoxBody] {

  override type M = EncryPaymentBox

  override def json: Json = ???

  override def serializer: BoxCompanionSerializer[EncryPaymentBox] = EncryPaymentBoxSerializer

}

object EncryPaymentBoxSerializer extends BoxCompanionSerializer[EncryPaymentBox] {
  val Length: Int = 8

  override def toBytes(obj: EncryPaymentBox): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[EncryPaymentBox] = ???
}
