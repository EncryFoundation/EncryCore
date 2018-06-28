package encry.modifiers.state.box

import encry.modifiers.serialization.BytesSerializable
import encry.view.state.Proposition
import scorex.crypto.authds._

trait Box[P <: Proposition] extends BytesSerializable {
  val value: Box.Amount
  val proposition: P

  val id: ADKey
}

object Box {
  type Amount = Long
}

