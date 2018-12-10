package encry.it.docker

import java.net.{InetAddress, InetSocketAddress, URL}
import com.typesafe.config.Config
import encry.EncryApp.settings
import encry.it.util.GlobalTimer
import encry.settings.{Constants, EncryAppSettings}
import org.asynchttpclient.Dsl.{config => clientConfig, _}
import org.asynchttpclient._
import org.encryfoundation.common.Algos
import org.encryfoundation.common.crypto.PublicKey25519
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}

import scala.concurrent.duration.FiniteDuration

abstract class Node(config: Config) extends AutoCloseable {

  val settings: EncryAppSettings = EncryAppSettings.fromConfig(config)
  val client: AsyncHttpClient = asyncHttpClient(
    clientConfig()
      .setKeepAlive(false)
      .setNettyTimer(GlobalTimer.instance))

  val keyPair: (PrivateKey, PublicKey) =
    Curve25519.createKeyPair(Algos.decode(settings.wallet.flatMap(_.seed).getOrElse("")).get)
  val publicKey = PublicKey25519(keyPair._2)
  val address: String = publicKey.address.address

  def nodeApiEndpoint: URL
  def apiKey: String

  /** An address which can be reached from the host running IT (may not match the declared address) */
  def networkAddress: InetSocketAddress

  override def close(): Unit = client.close()
}

object Node {

  implicit class NodeExt(val n: Node) extends AnyVal {
    def name: String = n.settings.network.nodeName
      .getOrElse(InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort)

    def publicKeyStr: String = n.publicKey.toString

    def blockDelay: FiniteDuration = Constants.Chain.DesiredBlockInterval
  }
}
