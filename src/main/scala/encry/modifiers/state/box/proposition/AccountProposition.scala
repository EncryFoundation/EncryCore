package encry.modifiers.state.box.proposition

import encry.account.{Account, AccountSerializer, Address}
import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.Proof
import encry.modifiers.state.box.proposition.EncryProposition.UnlockFailedException
import encrywm.lang.backend.env.{ESObject, ESValue}
import encrywm.lib.Types
import encrywm.lib.Types._
import io.circe.Encoder
import io.circe.syntax._
import scorex.core.serialization.Serializer

import scala.util.{Failure, Success, Try}

/**
  * This type of proposition requires non-interactive proof of knowledge of
  * `PrivateKey` corresponding to some `account.address`.
  */
case class AccountProposition(account: Account) extends EncryProposition {

  override type M = AccountProposition

  override val typeId: Byte = AccountProposition.TypeId

  override def serializer: Serializer[M] = AccountPropositionSerializer

  override def unlockTry(proof: Proof)(implicit ctx: Context): Try[Unit] =
    if (Account(ctx.transaction.accountPubKey.pubKeyBytes) != account) Failure(UnlockFailedException)
    else Success()

  override val esType: Types.ESProduct = Types.AccountProposition

  override def asVal: ESValue = ESValue(Types.AccountProposition.ident.toLowerCase, Types.AccountProposition)(convert)

  override def convert: ESObject = {
    val fields = Map(
      "typeId" -> ESValue("typeId", ESInt)(typeId.toInt),
      "accountAddress" -> ESValue("accountAddress", ESString)(account.address)
    )
    ESObject(Types.AccountProposition.ident, fields, esType)
  }
}

object AccountProposition {

  val TypeId: Byte = 2

  implicit val jsonEncoder: Encoder[AccountProposition] = (p: AccountProposition) => Map(
    "typeId" -> TypeId.toInt.asJson,
    "address" -> p.account.address.toString.asJson
  ).asJson

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
