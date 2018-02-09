package encry.modifiers.mempool

trait ModifierWithSizeLimit {

  val maxSize: Int

  val length: Int

  def validSize: Boolean = length <= maxSize
}
