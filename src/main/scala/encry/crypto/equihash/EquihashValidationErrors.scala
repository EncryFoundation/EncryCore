package encry.crypto.equihash

sealed trait EquihashValidationErrors
object EquihashValidationErrors {
  case class SolutionValidationError(err: String) extends EquihashValidationErrors
  case class VerifyingCandidateValidationError(err: String) extends EquihashValidationErrors
  case class GeneratingHeaderError(err: String) extends EquihashValidationErrors
}