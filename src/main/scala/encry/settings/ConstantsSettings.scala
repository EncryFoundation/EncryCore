package encry.settings

import org.encryfoundation.common.utils.constants.Constants

trait ConstantsSettings {
  val constants: Constants = EncryAppSettings.read().constants
}
