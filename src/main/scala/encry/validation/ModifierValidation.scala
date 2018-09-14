package encry.validation

import encry.consensus.ModifierSemanticValidity
import encry.validation.ValidationResult.{Invalid, Valid}
import org.encryfoundation.common.Algos

trait ModifierValidator {

  /** Start validation in Fail-Fast mode */
  def failFast: ValidationState = ValidationState(Valid, ValidationStrategy.FailFast)

  /** Start validation accumulating all the errors */
  def accumulateErrors: ValidationState = ValidationState(Valid, ValidationStrategy.AccumulateErrors)

  /** report recoverable modifier error that could be fixed by later retries */
  def error(errorMessage: String): Invalid = invalid(RecoverableModifierError(errorMessage))

  /** report non-recoverable modifier error that could be fixed by retries and requires modifier change */
  def fatal(errorMessage: String): Invalid = invalid(MalformedModifierError(errorMessage))

  /** unsuccessful validation with a given error */
  def invalid(error: ModifierError): Invalid = Invalid(Seq(error))

  /** successful validation */
  def success: Valid = Valid

  /** Shortcut method for the simple single-check validation.
    * If you need to validate against multiple checks, which is usual,
    * then use [[failFast]] and [[accumulateErrors]] to start the validation
    */
  def validate(condition: Boolean)(error: => Invalid): ValidationResult = accumulateErrors.validate(condition)(error).result

  /** Shortcut `require`-like method for the simple single-check validation with fatal error.
    * If you need to validate against multiple checks then use [[failFast]] and [[accumulateErrors]]
    */
  def demand(condition: Boolean, fatalError: => String): ValidationResult = validate(condition)(fatal(fatalError))

  /** Shortcut `require`-like method for the simple single-check validation with recoverable error.
    * If you need to validate against multiple checks then use [[failFast]] and [[accumulateErrors]]
    */
  def recoverable(condition: Boolean, recoverableError: => String): ValidationResult =
    validate(condition)(error(recoverableError))

}

object ModifierValidator extends ModifierValidator

/** This is the place where all the validation DSL lives */
case class ValidationState(result: ValidationResult, strategy: ValidationStrategy) {

  /** Reverse condition: Validate the condition is `false` or else return the `error` given */
  def validateNot(condition: => Boolean)(error: => Invalid): ValidationState = validate(!condition)(error)

  /** Validate the `id`s are equal. The `error` callback will be provided with detail on argument values */
  def validateEqualIds(given: => Array[Byte], expected: => Array[Byte])(error: String => Invalid): ValidationState =
    validate(given sameElements expected)(error(s"Given: ${Algos.encode(given)}, expected ${Algos.encode(expected)}"))

  /** Wrap semantic validity to the validation state: if semantic validity was not Valid, then return the `error` given
    */
  def validateSemantics(validity: => ModifierSemanticValidity)(error: => Invalid): ValidationState =
    validateNot(validity == ModifierSemanticValidity.Invalid)(error)

  /** Validate the condition is `true` or else return the `error` given */
  def validate(condition: => Boolean)(error: => Invalid): ValidationState = pass(if (condition) Valid else error)

  /** Create the next validation state as the result of given `operation` */
  def pass(operation: => ValidationResult): ValidationState = {
    result match {
      case Valid => copy(result = operation)
      case Invalid(_) if strategy.isFailFast => this
      case Invalid(_) => copy(result = result ++ operation)
    }
  }

  /** Shortcut `require`-like method for the simple validation with fatal error.
    * If you need more convenient checks, use `validate` methods.
    */
  def demand(condition: => Boolean, fatalError: => String): ValidationState =
    validate(condition)(ModifierValidator.fatal(fatalError))
}

/** The strategy indicates are we going to perform fail-fast or error-accumulative validation.
  * These two could be also mixed by nested validations.
  */
sealed abstract class ValidationStrategy(val isFailFast: Boolean)

object ValidationStrategy {

  object AccumulateErrors extends ValidationStrategy(false)

  object FailFast extends ValidationStrategy(true)

}