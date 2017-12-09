package encry.modifiers.mempool.box

import encry.modifiers.mempool.box.body.PaymentBoxBody
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.{JsonSerializable, Serializer}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition

import scala.util.Try

case class EncryPaymentBox(override val proposition: PublicKey25519Proposition,
                           override val nonce: Long,
                           override val body: PaymentBoxBody)
  extends EncryPublicKeyNoncedBox[PublicKey25519Proposition, PaymentBoxBody] {

  override def json: Json = ???

  override def serializer: Serializer[EncryPaymentBox] = EncryPaymentBoxSerializer

}

object EncryPaymentBoxSerializer extends Serializer[EncryPaymentBox] {

  override def toBytes(obj: EncryPaymentBox): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[EncryPaymentBox] = ???
}
