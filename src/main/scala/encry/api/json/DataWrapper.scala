package encry.api.json

trait DataWrapper[T] {

  def toBaseObj: T
}
