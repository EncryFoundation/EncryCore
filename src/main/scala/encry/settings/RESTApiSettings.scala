package encry.settings

import java.net.InetSocketAddress

import scala.concurrent.duration.FiniteDuration

case class RESTApiSettings(enabled: Boolean,
                           bindAddress: InetSocketAddress,
                           apiKeyHash: Option[String],
                           corsAllowedOrigin: Option[String],
                           timeout: FiniteDuration)