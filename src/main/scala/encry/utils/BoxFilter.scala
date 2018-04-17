package encry.utils

import encry.modifiers.state.box.{MonetaryBox, EncryBaseBox}

object BoxFilter {

  def filterAmountCarryingBxs(bxs: Seq[EncryBaseBox]): Seq[MonetaryBox] =
    bxs.foldLeft(Seq[MonetaryBox]())((acc, bx) => bx match {
      case acbx: MonetaryBox => acc :+ acbx
      case _ => acc
    })
}
