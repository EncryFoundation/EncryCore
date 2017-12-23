package encry.modifiers.state.box

import scorex.crypto.authds._
import scorex.crypto.encode.Base58

abstract class EncryBoxStateChangeOperation

case class Removal(boxId: ADKey) extends EncryBoxStateChangeOperation{
  override def toString: String = s"Removal(id: ${Base58.encode(boxId)})"
}

case class Insertion(box: Any) extends EncryBoxStateChangeOperation

case class EncryBoxStateChanges(operations: Seq[EncryBoxStateChangeOperation]){

  def toAppend: Seq[EncryBoxStateChangeOperation] = operations.filter {op =>
    if (op.isInstanceOf[Insertion]) true else false
  }

  def toRemove: Seq[EncryBoxStateChangeOperation] = operations.filter {op =>
    if (op.isInstanceOf[Removal]) true else false
  }
}