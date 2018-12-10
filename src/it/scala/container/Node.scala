package container

import org.asynchttpclient._
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration


class Node(val settings: EncrySettings, val nodeInfo: NodeInfo, override val client: AsyncHttpClient)
          (implicit override val ec: ExecutionContext) extends NodeApi with NetworkNodeApi {
  // todo after addresses will added
  //  val privateKey: String = config.getString("private-key")
  //  val publicKey: String = config.getString("public-key")
  //  val address: String = config.getString("address")
  //  val accountSeed: String = config.getString("account-seed")

  override protected val log: Logger =
    LoggerFactory.getLogger(s"${getClass.getName}.${settings.network.nodeName}")

  def nodeName: String = settings.network.nodeName
  def containerId: String = nodeInfo.containerId
  override val chainId: Char = 'I'
  override val networkNodeName: String = s"it-test-client-to-${nodeInfo.networkIpAddress}"
  override val restAddress: String = "localhost"
  override val networkAddress: String = "localhost"
  override val nodeRestPort: Int = nodeInfo.hostRestApiPort
  override val networkPort: Int = nodeInfo.hostNetworkPort
  override val blockDelay: FiniteDuration = settings.chainSettings.blockInterval

}
