package encry.modifiers.mempool.directive

import encry.modifiers.mempool.directive.Directive.DirTypeId
import encry.modifiers.state.box.EncryBaseBox
import io.circe.Encoder
import scorex.core.serialization.BytesSerializable
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.hash.Digest32

// Directive is sub-modifier of the state.
trait Directive extends BytesSerializable {

  val typeId: DirTypeId

  val idx: Int

  val cost: Amount

  val isValid: Boolean

  def boxes(digest: Digest32): Seq[EncryBaseBox]
}

object Directive {

  type DirTypeId = Byte

  implicit val jsonEncoder: Encoder[Directive] = {
    case td: TransferDirective => TransferDirective.jsonEncoder(td)
    case cd: CoinbaseDirective => CoinbaseDirective.jsonEncoder(cd)
    case apk: AddPubKeyInfoDirective => AddPubKeyInfoDirective.jsonEncoder(apk)
  }
}
