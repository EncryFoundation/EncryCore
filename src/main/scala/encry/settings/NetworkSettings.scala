package encry.settings

import java.net.InetSocketAddress
import scala.concurrent.duration.FiniteDuration

case class NetworkSettings(nodeName: String,
                           addedMaxDelay: Option[FiniteDuration],
                           networkChunkSize: Int,
                           localOnly: Boolean,
                           knownPeers: Seq[InetSocketAddress],
                           bindAddress: InetSocketAddress,
                           maxConnections: Int,
                           connectionTimeout: FiniteDuration,
                           upnpEnabled: Boolean,
                           upnpGatewayTimeout: Option[FiniteDuration],
                           upnpDiscoverTimeout: Option[FiniteDuration],
                           declaredAddress: Option[InetSocketAddress],
                           handshakeTimeout: FiniteDuration,
                           deliveryTimeout: FiniteDuration,
                           maxDeliveryChecks: Int,
                           appVersion: String,
                           maxInvObjects: Int,
                           connectOnlyWithKnownPeers: Boolean,
                           modifierDeliverTimeCheck: FiniteDuration,
                           syncInterval: FiniteDuration,
                           syncTimeout: Option[FiniteDuration],
                           syncPacketLength: Int
                          )
