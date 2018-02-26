package encry.modifiers.mempool.directive

import com.google.common.primitives.{Bytes, Ints, Longs}
import encry.account.{Account, Address}
import encry.modifiers.mempool.EncryTransaction.Amount
import encry.modifiers.mempool.directive.Directive.DirTypeId
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import encry.settings.Algos
import encry.utils.Utils
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

case class TransferDirective(address: Address,
                             amount: Amount,
                             override val idx: Int) extends Directive with AmountTransferingDirective {

  override type M = TransferDirective

  override val typeId: DirTypeId = TransferDirective.TypeId

  override val hash: Digest32 =
    Algos.hash(Account.decodeAddress(address) ++ Longs.toByteArray(amount) ++ Ints.toByteArray(idx))

  override def boxes(digest: Digest32): Seq[EncryBaseBox] =
    Seq(AssetBox(address, Utils.nonceFromDigest(digest ++ Ints.toByteArray(idx)), amount))

  override val cost: Amount = 4

  override lazy val isValid: Boolean = amount > 0 && Account.validAddress(address)

  override def serializer: Serializer[M] = TransferDirectiveSerializer

  override def json: Json = Map(
    "typeId" -> typeId.asJson,
    "verboseType" -> "TRANSFER".asJson,
    "address" -> address.toString.asJson,
    "amount" -> amount.asJson,
    "idx" -> idx.asJson
  ).asJson
}

object TransferDirective {

  val TypeId: DirTypeId = 1.toByte
}

object TransferDirectiveSerializer extends Serializer[TransferDirective] {

  override def toBytes(obj: TransferDirective): Array[Byte] =
    Bytes.concat(
      Account.decodeAddress(obj.address),
      Longs.toByteArray(obj.amount),
      Ints.toByteArray(obj.idx)
    )

  override def parseBytes(bytes: Array[Byte]): Try[TransferDirective] = Try {
    val address = Address @@ Base58.encode(bytes.take(Account.AddressLength))
    val amount = Longs.fromByteArray(bytes.slice(Account.AddressLength, Account.AddressLength + 8))
    val idx = Ints.fromByteArray(bytes.takeRight(4))
    TransferDirective(address, amount, idx)
  }
}
