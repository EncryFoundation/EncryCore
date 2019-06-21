package encry.utils.implicits

import cats.kernel.Monoid
import org.encryfoundation.common.validation.ValidationResult
import org.encryfoundation.common.validation.ValidationResult.Valid

object Validation {

  implicit def validationResMonoid: Monoid[ValidationResult] = new Monoid[ValidationResult] {

    override def empty: ValidationResult = Valid

    override def combine(x: ValidationResult, y: ValidationResult): ValidationResult = x ++ y
  }
}
