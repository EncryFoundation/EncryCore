package encry.modifiers.mempool.directive

import com.google.common.primitives.{Ints, Longs}
import encry.account.Address
import encry.modifiers.mempool.directive.Directive.DTypeId
import encry.modifiers.state.box.EncryBaseBox
import encry.settings.Constants
import encry.view.history.Height
import encry.view.state.UtxoState
import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.hash.Digest32

import scala.util.Try

case class CoinbaseDirective(height: Height) extends Directive {

  override type M = CoinbaseDirective

  // Coinbase directive always comes first in transaction.
  override val idx: Int = 0

  override val typeId: DTypeId = CoinbaseDirective.TypeId

  override def boxes(digest: Digest32): Seq[EncryBaseBox] =
    Seq(UtxoState.supplyBoxesAt(height, seed = Longs.fromByteArray(digest.take(8))))

  override val cost: Amount = 0

  override val isValid: Boolean = height > Constants.Chain.PreGenesisHeight

  override def serializer: Serializer[M] = CoinbaseDirectiveSerializer
}

object CoinbaseDirective {

  val TypeId: DTypeId = 0.toByte

  implicit val jsonEncoder: Encoder[CoinbaseDirective] = (d: CoinbaseDirective) => Map(
    "typeId" -> d.typeId.asJson,
    "height" -> d.height.toString.asJson,
    "idx" -> d.idx.asJson
  ).asJson

  implicit val jsonDecoder: Decoder[CoinbaseDirective] = (c: HCursor) => for {
    height <- c.downField("heigth").as[Int]
  } yield {
    CoinbaseDirective(
      Height @@ height
    )
  }
}

object CoinbaseDirectiveSerializer extends Serializer[CoinbaseDirective] {

  override def toBytes(obj: CoinbaseDirective): Array[Byte] = Ints.toByteArray(obj.height)

  override def parseBytes(bytes: Array[Byte]): Try[CoinbaseDirective] =
    Try(CoinbaseDirective(Height @@ Ints.fromByteArray(bytes)))
}
