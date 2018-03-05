package encry.api.templates

import scala.util.Try

trait BaseTemplate[T] {

  def origin: Try[T]
}
