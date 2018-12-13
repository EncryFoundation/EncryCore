package encry.it.docker

import java.net.{InetAddress, InetSocketAddress, URL}
import java.nio.file.Paths
import java.util.Collections._
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{Properties, UUID, List => JList, Map => JMap}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.google.common.primitives.Ints
import com.spotify.docker.client.DockerClient.RemoveContainerParam
import com.spotify.docker.client.messages.EndpointConfig.EndpointIpamConfig
import com.spotify.docker.client.messages._
import com.spotify.docker.client.{DefaultDockerClient, DockerClient}
import com.typesafe.config.ConfigFactory._
import com.typesafe.config.{Config, ConfigRenderOptions}
import com.typesafe.scalalogging.StrictLogging
import org.asynchttpclient.Dsl._

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random
import scala.util.control.NonFatal

case class Docker(suiteConfig: Config = empty,
             tag: String = "",
             enableProfiling: Boolean = false) extends AutoCloseable with StrictLogging {

  import Docker._

  val client = DefaultDockerClient.fromEnv().build()
  val nodes = ConcurrentHashMap.newKeySet[Node]()

  private val http = asyncHttpClient(config()
    .setMaxConnections(50)
    .setMaxConnectionsPerHost(10)
    .setMaxRequestRetry(1)
    .setReadTimeout(10000)
    .setRequestTimeout(10000))

  private def uuidShort: String = UUID.randomUUID().hashCode().toHexString
  private val networkName = Docker.networkNamePrefix + uuidShort
  private val networkSeed = Random.nextInt(0x100000) << 4 | 0x0A000000
  private val networkPrefix = s"${InetAddress.getByAddress(Ints.toByteArray(networkSeed)).getHostAddress}/28"
  private val isStopped = new AtomicBoolean(false)
  val network = createNetwork(3)

  def waitForStartupBlocking(nodes: List[Node]): List[Node] = {
    logger.debug("Waiting for nodes to start")
    Await.result(waitForStartup(nodes), nodes.size * 90.seconds)
  }

  def waitForStartup(nodes: List[Node]): Future[List[Node]] = {
    Future.sequence(nodes map { _.waitForStartup })
  }

  def ipForNode(nodeNumber: Int): String = {
    val addressBytes = Ints.toByteArray(nodeNumber & 0xF | networkSeed)
    InetAddress.getByAddress(addressBytes).getHostAddress
  }

  def createNetwork(maxRetry: Int = 5): Network = try {
    val params = DockerClient.ListNetworksParam.byNetworkName(networkName)
    val networkOpt = client.listNetworks(params).asScala.headOption
    networkOpt match {
      case Some(network) =>
        logger.info(s"Network ${network.name()} (id: ${network.id()}) is created for $tag, " +
          s"ipam: ${ipamToString(network)}")
        network
      case None =>
        logger.debug(s"Creating network $networkName for $tag")
        // Specify the network manually because of race conditions: https://github.com/moby/moby/issues/20648
        val r = client.createNetwork(buildNetworkConfig())
        Option(r.warnings()).foreach(logger.warn(_))
        createNetwork(maxRetry - 1)
    }
  } catch {
    case NonFatal(e) =>
      logger.warn(s"Can not create a network for $tag", e)
      if (maxRetry == 0) throw e else createNetwork(maxRetry - 1)
  }

  private def buildNetworkConfig(): NetworkConfig = {
    val config = IpamConfig.create(networkPrefix, networkPrefix, ipForNode(0xE))
    val ipam = Ipam.builder()
      .driver("default")
      .config(Seq(config).asJava)
      .build()

    NetworkConfig.builder()
      .name(networkName)
      .ipam(ipam)
      .checkDuplicate(true)
      .build()
  }

  def ipamToString(network: Network): String =
    network
      .ipam()
      .config().asScala
      .map { n => s"subnet=${n.subnet()}, ip range=${n.ipRange()}" }
      .mkString(", ")

  private def endpointConfigFor(nodeName: String): EndpointConfig = {
    val nodeNumber = nodeName.replace("node", "").toInt
    val ip         = ipForNode(nodeNumber)
    EndpointConfig
      .builder()
      .ipAddress(ip)
      .ipamConfig(EndpointIpamConfig.builder().ipv4Address(ip).build())
      .build()
  }

  private def dumpContainers(containers: java.util.List[Container], label: String = "Containers"): Unit = {
    val x =
      if (containers.isEmpty) "No"
      else
        "\n" + containers.asScala
          .map { x =>
            s"Container(${x.id()}, status: ${x.status()}, names: ${x.names().asScala.mkString(", ")})"
          }
          .mkString("\n")

    logger.debug(s"$label: $x")
  }

  def startNodeInternal(nodeConfig: Config): Node =
    try {
      val restApiPort = nodeConfig.getString("encry.restApi.bindAddress").split(":").last
      val networkPort = nodeConfig.getString("encry.network.bindAddress").split(":").last

      val portBindings = new ImmutableMap.Builder[String, java.util.List[PortBinding]]()
        .put(s"$ProfilerPort", singletonList(PortBinding.randomPort("0.0.0.0")))
        .put(restApiPort, singletonList(PortBinding.randomPort("0.0.0.0")))
        .put(networkPort, singletonList(PortBinding.randomPort("0.0.0.0")))
        .build()

      val hostConfig = HostConfig
        .builder()
        .portBindings(portBindings)
        .build()

      val nodeName   = nodeConfig.getString("encry.network.nodeName")
      val nodeNumber = nodeName.replace("node", "").toInt
      val ip         = ipForNode(nodeNumber)

      val javaOptions = Option(System.getenv("CONTAINER_JAVA_OPTS")).getOrElse("")

      val configCommandLine = renderProperties(asProperties(nodeConfig))

      val containerConfig = ContainerConfig
        .builder()
        .attachStdin(true)
        .image("bromel777/encry-core:latest")
        .exposedPorts(s"$ProfilerPort", restApiPort, networkPort)
        .networkingConfig(ContainerConfig.NetworkingConfig.create(Map(
          network.name() -> endpointConfigFor(nodeName)
        ).asJava))
        .hostConfig(hostConfig)
        .env(s"OPTS=$configCommandLine")
        .build()

      val containerId = {
        val containerName = s"${network.name()}-$nodeName"
        dumpContainers(
          client.listContainers(DockerClient.ListContainersParam.filter("name", containerName)),
          "Containers with same name"
        )

        logger.debug(s"Creating container $containerName at $ip with options: $javaOptions")
        val r = client.createContainer(containerConfig, containerName)
        Option(r.warnings().asScala).toSeq.flatten.foreach(logger.warn(_))
        r.id()
      }

      client.startContainer(containerId)
      val containerInfo = client.inspectContainer(containerId)
      val ports = containerInfo.networkSettings().ports()
      val hostRestApiPort = extractHostPort(ports, 9051) //get port from settings
      val node = new Node(nodeConfig, hostRestApiPort, containerId, http)
      nodes.add(node)
      logger.debug(s"Started $containerId -> ${node.name}")
      node
    } catch {
      case NonFatal(e) =>
        logger.error("Can't start a container", e)
        dumpContainers(client.listContainers())
        throw e
    }

  def extractHostPort(portBindingMap: JMap[String, JList[PortBinding]], containerPort: Int): Int =
    portBindingMap.get(s"$containerPort/tcp").get(0).hostPort().toInt

  def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  override def close(): Unit = {
    if (isStopped.compareAndSet(false, true)) {
      logger.info("Stopping containers")
      nodes.asScala.foreach { node =>
        node.close()
        client.stopContainer(node.containerId, 0)
      }

      nodes.asScala.foreach(_.client.close())

      //saveNodeLogs()

      nodes.asScala foreach { node =>
        client.removeContainer(node.containerId, RemoveContainerParam.forceKill())
      }
      client.removeNetwork(network.id())
      client.close()

//      localDataVolumeOpt.foreach { path =>
//        val dataVolume = new File(path)
//        FileUtils.deleteDirectory(dataVolume)
//      }
    }
  }
}

object Docker {
  private val ContainerRoot      = Paths.get("/opt/encry")
  private val ProfilerController = ContainerRoot.resolve("yjp-controller-api-redist.jar")
  private val ProfilerPort       = 10001
  private val jsonMapper         = new ObjectMapper
  private val propsMapper        = new JavaPropsMapper
  val networkNamePrefix: String = "itest-"

  val configTemplate = parseResources("application.conf")

  def apply(owner: Class[_]): Docker = new Docker(tag = owner.getSimpleName)

  private def asProperties(config: Config): Properties = {
    val jsonConfig = config.root().render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  private def renderProperties(p: Properties) =
    p.asScala
      .map {
        case (k, v) if v.contains(" ") => k -> s""""$v""""
        case x                         => x
      }
      .map { case (k, v) => s"-D$k=$v" }
      .mkString(" ")

  private def extractHostPort(m: JMap[String, JList[PortBinding]], containerPort: Int) =
    m.get(s"$containerPort/tcp").get(0).hostPort().toInt

  case class NodeInfo(nodeApiEndpoint: URL,
                      hostNetworkAddress: InetSocketAddress,
                      containerNetworkAddress: InetSocketAddress)
}

