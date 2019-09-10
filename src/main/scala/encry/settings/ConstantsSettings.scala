package encry.settings

import org.encryfoundation.common.utils.constants.Constants

trait ConstantsSettings {
  lazy val constants: Constants = EncryAppSettings.read().constants
}
