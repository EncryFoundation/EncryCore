package encry.api.models

trait BaseModel[T] {

  def toBaseObjOpt: Option[T]
}
