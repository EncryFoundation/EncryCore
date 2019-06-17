package encry.settings

import scala.concurrent.duration.FiniteDuration

case class BlackListSettings(banTime: FiniteDuration, cleanupTime: FiniteDuration)