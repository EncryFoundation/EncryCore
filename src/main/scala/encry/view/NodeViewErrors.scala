package encry.view

object NodeViewErrors {

  sealed trait ModifierApplyError
  object ModifierApplyError {
    case class StateModifierApplyError(msg: String) extends ModifierApplyError
    case class HistoryApplyError(msg: String) extends ModifierApplyError
  }
}
