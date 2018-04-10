package encry.utils

import encry.modifiers.state.box.{AmountCarryingBox, EncryBaseBox}

object BoxFilter {

  def filterAmountCarryingBxs(bxs: Seq[EncryBaseBox]): Seq[AmountCarryingBox] =
    bxs.foldLeft(Seq[AmountCarryingBox]())((acc, bx) => bx match {
      case acbx: AmountCarryingBox => acc :+ acbx
      case _ => acc
    })
}
