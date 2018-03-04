package encry.modifiers.mempool.directive

import com.google.common.primitives.{Ints, Longs}
import encry.modifiers.mempool.directive.Directive.DirTypeId
import encry.modifiers.state.box.EncryBaseBox
import encry.settings.Constants
import encry.view.history.Height
import encry.view.state.UtxoState
import io.circe.Json
import io.circe.syntax._
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.hash.Digest32

import scala.util.Try

case class CoinbaseDirective(height: Height) extends Directive {

  override type M = CoinbaseDirective

  // Coinbase directive always comes first in transaction.
  override val idx: Int = 0

  override val typeId: DirTypeId = CoinbaseDirective.TypeId

  override def boxes(digest: Digest32): Seq[EncryBaseBox] =
    Seq(UtxoState.supplyBoxesAt(height, seed = Longs.fromByteArray(digest.take(8))))

  override val cost: Amount = 0

  override val isValid: Boolean = height > Constants.Chain.preGenesisHeight

  override def serializer: Serializer[M] = CoinbaseDirectiveSerializer

  override def json: Json = Map(
    "typeId" -> typeId.asJson,
    "verboseType" -> "COINBASE".asJson,
    "height" -> height.toString.asJson,
    "idx" -> idx.asJson
  ).asJson
}

object CoinbaseDirective {

  val TypeId: DirTypeId = 0.toByte
}

object CoinbaseDirectiveSerializer extends Serializer[CoinbaseDirective] {

  override def toBytes(obj: CoinbaseDirective): Array[Byte] = Ints.toByteArray(obj.height)

  override def parseBytes(bytes: Array[Byte]): Try[CoinbaseDirective] =
    Try(CoinbaseDirective(Height @@ Ints.fromByteArray(bytes)))
}
