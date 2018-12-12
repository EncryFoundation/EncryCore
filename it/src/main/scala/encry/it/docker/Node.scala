package encry.it.docker

import java.net.{InetAddress, InetSocketAddress, URL}

import com.typesafe.config.Config
import com.typesafe.scalalogging.{Logger, StrictLogging}
import encry.EncryApp.settings
import encry.it.api.HttpApi
import encry.it.util.GlobalTimer
import encry.settings.{Constants, EncryAppSettings}
import org.asynchttpclient.Dsl.{config => clientConfig, _}
import org.asynchttpclient._
import org.encryfoundation.common.Algos
import org.encryfoundation.common.crypto.PublicKey25519
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}

import scala.concurrent.duration.FiniteDuration

case class Node(config: Config,
                port: Int,
                containerId: String,
                client: AsyncHttpClient) extends AutoCloseable with StrictLogging with HttpApi {

  val settings: EncryAppSettings = EncryAppSettings.fromConfig(config)
  val keyPair: (PrivateKey, PublicKey) =
    Curve25519.createKeyPair(Algos.decode(settings.wallet.flatMap(_.seed).getOrElse("")).get)
  val publicKey = PublicKey25519(keyPair._2)
  val address: String = publicKey.address.address

  def nodeApiEndpoint: URL = new URL("http://0.0.0.0:9051")
  def apiKey: String = "key"

  /** An address which can be reached from the host running IT (may not match the declared address) */
  def networkAddress: InetSocketAddress = new InetSocketAddress("0.0.0.0", 1234)

  override def close(): Unit = client.close()

  override def restAddress: String = "localhost"

  override def nodeRestPort: Int = port
}

object Node {

  implicit class NodeExt(val n: Node) extends AnyVal {
    def name: String = n.settings.network.nodeName
      .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort)

    def publicKeyStr: String = n.publicKey.toString

    def blockDelay: FiniteDuration = Constants.Chain.DesiredBlockInterval
  }
}
