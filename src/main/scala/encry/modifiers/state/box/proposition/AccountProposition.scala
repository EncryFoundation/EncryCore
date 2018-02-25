package encry.modifiers.state.box.proposition

import encry.account.{Account, AccountSerializer, Address}
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.Proposition

import scala.util.Try

// This type of proposition requires non-interactive proof of knowledge of
// `PrivateKey` corresponding to `account.address`.
case class AccountProposition(account: Account) extends Proposition {

  override type M = AccountProposition

  override def serializer: Serializer[M] = AccountPropositionSerializer
}

object AccountPropositionSerializer extends Serializer[AccountProposition] {

  val Length: Int = Account.AddressLength + 1

  override def toBytes(obj: AccountProposition): Array[Byte] = AccountProposition.TypeId +: obj.account.bytes

  override def parseBytes(bytes: Array[Byte]): Try[AccountProposition] = Try {
    assert(bytes.head == AccountProposition.TypeId && bytes.tail.length == Account.AddressLength)
    AccountSerializer.parseBytes(bytes.tail).map(AccountProposition.apply)
      .getOrElse(throw new Exception("Deserialization failed."))
  }
}

object AccountProposition {

  val TypeId: Byte = 1

  def apply(address: Address): AccountProposition = AccountProposition(Account(address))
}
