package encry.modifiers.state.box

import org.encryfoundation.common.serialization.BytesSerializable
import org.encryfoundation.common.transaction.Proposition
import scorex.crypto.authds._

trait Box[P <: Proposition] extends BytesSerializable {
  val value: Box.Amount
  val proposition: P

  val id: ADKey
}

object Box {
  type Amount = Long
}

