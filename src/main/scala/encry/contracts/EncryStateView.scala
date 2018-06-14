package encry.contracts

import encry.view.history.Height
import org.encryfoundation.prismlang.core.wrapped.{PObject, PValue}
import org.encryfoundation.prismlang.core.{PConvertible, Types}
import scorex.crypto.authds.ADDigest

case class EncryStateView(height: Height, lastBlockTimestamp: Long, stateDigest: ADDigest) extends PConvertible {

  override val esType: Types.Product = Types.EncryState

  override def asVal: PValue = PValue(esType)(convert)

  override def convert: PObject = {
    val fields = Map(
      "height" -> PValue(Types.PInt)(height),
      "lastBlockTimestamp" -> PValue(Types.PInt)(lastBlockTimestamp),
      "stateDigest" -> PValue(Types.PCollection.ofByte)(stateDigest)
    )
    PObject(fields, esType)
  }
}
