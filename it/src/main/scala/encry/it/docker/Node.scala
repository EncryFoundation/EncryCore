package encry.it.docker

import java.net.{InetAddress, InetSocketAddress, URL}

import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.settings
import encry.it.api.HttpApi
import encry.it.util.KeyHelper.createPrivKey
import encry.settings.EncryAppSettings
import org.asynchttpclient._
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519}

import encry.settings.EncryAppSettings.read.constants

import scala.concurrent.duration.FiniteDuration

case class Node(config: Config,
                restApiPort: Int,
                containerId: String,
                nodeIp: String,
                nodePort: Int,
                client: AsyncHttpClient) extends AutoCloseable with StrictLogging with HttpApi {

  val settings: EncryAppSettings = EncryAppSettings.fromConfig(config)
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

  implicit class NodeExt(val n: Node) extends AnyVal {
    def name: String = n.settings.network.nodeName
      .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort)

    def publicKeyStr: String = n.publicKey.toString

    def blockDelay: FiniteDuration = settings.constants.DesiredBlockInterval
  }
}
