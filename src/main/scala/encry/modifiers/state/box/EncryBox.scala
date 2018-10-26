package encry.modifiers.state.box

trait EncryBox[P <: EncryProposition] extends EncryBaseBox {

  override val proposition: P

}

object EncryBox {

  type BxTypeId = Byte

  val BoxIdSize = 32
}
