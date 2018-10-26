package encry.settings

import java.net.InetSocketAddress

import scala.concurrent.duration.FiniteDuration

case class RESTApiSettings(enabled: Option[Boolean],
                           bindAddress: InetSocketAddress,
                           token: Option[String],
                           corsAllowedOrigin: Option[String],
                           timeout: FiniteDuration)