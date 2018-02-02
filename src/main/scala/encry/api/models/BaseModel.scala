package encry.api.models

trait BaseModel[T] {

  def toBaseObj: Option[T]
}
