package encry.settings

import java.net.InetSocketAddress
import scala.concurrent.duration.FiniteDuration

case class NetworkSettings(nodeName: Option[String],
                           addedMaxDelay: Option[FiniteDuration],
                           networkChunkSize: Int,
                           localOnly: Option[Boolean],
                           knownPeers: Seq[InetSocketAddress],
                           bindAddress: InetSocketAddress,
                           maxConnections: Int,
                           connectionTimeout: FiniteDuration,
                           declaredAddress: Option[InetSocketAddress],
                           handshakeTimeout: FiniteDuration,
                           deliveryTimeout: FiniteDuration,
                           maxDeliveryChecks: Int,
                           appVersion: String,
                           maxInvObjects: Int,
                           connectOnlyWithKnownPeers: Option[Boolean],
                           modifierDeliverTimeCheck: FiniteDuration,
                           syncInterval: FiniteDuration,
                           syncTimeout: Option[FiniteDuration],
                           syncPacketLength: Int,
                           maxNumberOfReConnections: Int)