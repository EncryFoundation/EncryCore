package encry.utils

import encry.modifiers.state.box.{AmountCarryingBox, EncryBaseBox, PubKeyInfoBox}

object BoxFilter {

  def filterAmountCarryingBxs(bxs: Seq[EncryBaseBox]): Seq[AmountCarryingBox] =
    bxs.foldLeft(Seq[AmountCarryingBox]())((acc, bx) => bx match {
      case acbx: AmountCarryingBox => acc :+ acbx
      case _ => acc
    })

  def filterPubKeyInfoBxs(bxs: Seq[EncryBaseBox]): Seq[PubKeyInfoBox] =
    bxs.foldLeft(Seq[PubKeyInfoBox]())((acc, bx) => bx match {
      case pib: PubKeyInfoBox => acc :+ pib
      case _ => acc
    })
}
