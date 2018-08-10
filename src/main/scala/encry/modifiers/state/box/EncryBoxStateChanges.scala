package encry.modifiers.state.box

import org.encryfoundation.common.Algos
import scorex.crypto.authds._

abstract class EncryBoxStateChangeOperation

case class Removal(boxId: ADKey) extends EncryBoxStateChangeOperation {

  override def toString: String = s"Removal(id: ${Algos.encode(boxId)})"
}

case class Insertion(box: EncryBaseBox) extends EncryBoxStateChangeOperation {

  override def toString: String = s"Insertion(id: ${Algos.encode(box.id)})"
}

case class EncryBoxStateChanges(operations: Seq[EncryBoxStateChangeOperation]){

  def toAppend: Seq[EncryBoxStateChangeOperation] = operations.filter(_.isInstanceOf[Insertion])

  def toRemove: Seq[EncryBoxStateChangeOperation] = operations.filter(_.isInstanceOf[Removal])
}
