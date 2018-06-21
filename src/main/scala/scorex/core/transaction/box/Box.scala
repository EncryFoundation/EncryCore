package scorex.core.transaction.box

import encry.modifiers.BytesSerializable
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds._

/**
  * Box is a state element locked by some proposition.
  */
trait Box[P <: Proposition] extends BytesSerializable {
  val value: Box.Amount
  val proposition: P

  val id: ADKey
}

object Box {
  type Amount = Long
}

