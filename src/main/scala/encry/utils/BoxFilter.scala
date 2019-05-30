package encry.utils

import org.encryfoundation.common.modifiers.state.box.{EncryBaseBox, MonetaryBox}

object BoxFilter {

  def filterAmountCarryingBxs(bxs: Seq[EncryBaseBox]): Seq[MonetaryBox] =
    bxs.foldLeft(Seq[MonetaryBox]())((acc, bx) => bx match {
      case acbx: MonetaryBox => acc :+ acbx
      case _ => acc
    })
}