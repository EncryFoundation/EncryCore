package encry.modifiers.state.box.proposition

import encry.account.Address
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.PublicKey25519Proposition.{ChecksumLength, calcCheckSum, _}
import scorex.core.transaction.box.proposition.{Proposition, PublicKey25519Proposition}
import scorex.crypto.encode.Base58

import scala.util.Try

// Holds the wallet address, which responds to some `publicKey`.
// Should be used with `scorex.core.transaction.box.proposition.PublicKey25519Proposition`.
case class AddressProposition(address: Address) extends Proposition {

  override type M = AddressProposition

  def verify(proposition: PublicKey25519Proposition): Boolean = address == proposition.address

  override def serializer: Serializer[AddressProposition] = AddressPropositionSerializer

  override lazy val bytes: Array[Byte] = AddressProposition.getAddrBytes(address)
}

object AddressProposition {

  def getAddrBytes(address: Address): Array[Byte] = Base58.decode(address).get

  // TODO: Use this in transaction.semanticValidity.
  def validAddress(address: Address): Boolean = {
    val addrBytes: Array[Byte] = getAddrBytes(address)
      if (addrBytes.length != AddressLength) false
      else {
        val checkSum = addrBytes.takeRight(ChecksumLength)
        val checkSumGenerated = calcCheckSum(addrBytes.dropRight(ChecksumLength))
        if (checkSum.sameElements(checkSumGenerated)) true
        else false
      }
    }
}

object AddressPropositionSerializer extends Serializer[AddressProposition] {

  override def toBytes(obj: AddressProposition): Array[Byte] = Base58.decode(obj.address).get

  override def parseBytes(bytes: Array[Byte]): Try[AddressProposition] = Try {
    new AddressProposition(Address @@ Base58.encode(bytes))
  }
}
