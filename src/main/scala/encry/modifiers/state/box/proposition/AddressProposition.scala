package encry.modifiers.state.box.proposition

import encry.crypto.Address
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.signatures.{PublicKey, Signature}

import scala.util.Try

class AddressProposition(val addrBytes: Address) extends Proposition {

  override type M = AddressProposition

  // TODO: Проверка соответствия адреса публичному ключу.
  def verify(message: Array[Byte], signature: Signature, pubKeyBytes: PublicKey): Boolean = ???

  override def serializer: Serializer[AddressProposition] = AddressPropositionSerializer

}

object AddressPropositionSerializer extends Serializer[AddressProposition] {

  override def toBytes(obj: AddressProposition): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[AddressProposition] = ???
}
