package encry.modifiers.state.box.proposition

import encry.account.{Account, AccountSerializer, Address}
import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import scorex.core.serialization.Serializer

import scala.util.{Failure, Success, Try}

// This type of proposition requires non-interactive proof of knowledge of
// `PrivateKey` corresponding to `account.address`.
case class AccountProposition(account: Account) extends EncryProposition {

  override type M = AccountProposition

  override def serializer: Serializer[M] = AccountPropositionSerializer

  override def unlockTry(proof: Proof)(implicit ctx: Context): Try[Unit] =
    if (Account(ctx.transaction.accountPubKey.pubKeyBytes) != account) Failure(new Error("Unlock failed"))
    else Success()
}

object AccountProposition {

  val TypeId: Byte = 2

  def apply(address: Address): AccountProposition = AccountProposition(Account(address))
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
