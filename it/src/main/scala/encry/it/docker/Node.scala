package encry.it.docker

import java.net.{InetAddress, InetSocketAddress, URL}

import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.it.api.HttpApi
import encry.utils.Utils.createPrivKey
import encry.settings.Settings
import org.asynchttpclient._
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}

import scala.concurrent.duration.FiniteDuration

case class Node(config: Config,
                restApiPort: Int,
                containerId: String,
                nodeIp: String,
                nodePort: Int,
                client: AsyncHttpClient) extends AutoCloseable with StrictLogging with HttpApi with Settings {

  val privKey: PrivateKey25519 = createPrivKey(Some(settings.wallet.flatMap(_.seed).getOrElse("")))
  val publicKey: PublicKey25519 = privKey.publicImage
  val address: String = publicKey.address.address

  def nodeApiEndpoint: URL = new URL("http://0.0.0.0:9051")
  def apiKey: String = "key"

  /** An address which can be reached from the host running IT (may not match the declared address) */
  def networkAddress: InetSocketAddress = new InetSocketAddress("0.0.0.0", 1234)

  override def close(): Unit = client.close()

  override def restAddress: String = "localhost"

  override def nodeRestPort: Int = restApiPort
}

object Node {

  implicit class NodeExt(val node: Node) extends AnyVal {
    def name: String = node.settings.network.nodeName
      .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + node.settings.network.bindAddress.getPort)

    def publicKeyStr: String = node.publicKey.toString

    def blockDelay: FiniteDuration = node.settings.constants.DesiredBlockInterval
  }
}
