package encry.it.docker

import java.io.FileOutputStream
import java.lang
import java.net.{InetAddress, InetSocketAddress, URL}
import java.nio.file.{Files, Paths}
import java.util.Collections._
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{Collections, Properties, UUID, List => JList, Map => JMap}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.google.common.primitives.Ints
import com.spotify.docker.client.DockerClient.RemoveContainerParam
import com.spotify.docker.client.messages.EndpointConfig.EndpointIpamConfig
import com.spotify.docker.client.messages._
import com.spotify.docker.client.{DefaultDockerClient, DockerClient}
import com.typesafe.config.ConfigFactory._
import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}
import com.typesafe.scalalogging.StrictLogging
import encry.it.configs.Configs
import encry.settings.EncryAppSettings
import org.asynchttpclient.Dsl._

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, blocking}
import scala.util.{Random, Try}
import scala.util.control.NonFatal

case class Docker(suiteConfig: Config = empty,
             tag: String = "",
             enableProfiling: Boolean = false) extends AutoCloseable with StrictLogging {

  import Docker._

  val client: DefaultDockerClient = DefaultDockerClient.fromEnv().build()
  val nodes: ConcurrentHashMap.KeySetView[Node, lang.Boolean] = ConcurrentHashMap.newKeySet[Node]()

  private val http = asyncHttpClient(config()
    .setMaxConnections(50)
    .setMaxConnectionsPerHost(10)
    .setMaxRequestRetry(1)
    .setReadTimeout(10000)
    .setRequestTimeout(10000))

  private def uuidShort: String = UUID.randomUUID().hashCode().toHexString
  private val networkName: String = Docker.networkNamePrefix + uuidShort
  private val networkSeed: Int = Random.nextInt(0x100000) << 4 | 0x0A000000
  private val networkPrefix: String = s"${InetAddress.getByAddress(Ints.toByteArray(networkSeed)).getHostAddress}/28"
  private val isStopped: AtomicBoolean = new AtomicBoolean(false)
  val network: Network = createNetwork(3)

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

  private def connectToNetwork(containerId: String, ip: String): AttachedNetwork = {
    client.connectToNetwork(
      network.id(),
      NetworkConnection
        .builder()
        .containerId(containerId)
        .endpointConfig(endpointConfigFor(ip))
        .build()
    )
    logger.info(s"Connecting to network. ContainerId: $containerId and ip: $ip")
    waitForNetwork(containerId)
  }

  @tailrec private def waitForNetwork(containerId: String, maxTry: Int = 5): AttachedNetwork = {
    def errMsg = s"Container $containerId has not connected to the network ${network.name()}"
    val containerInfo = client.inspectContainer(containerId)
    val networks = containerInfo.networkSettings().networks().asScala
    if (networks.contains(network.name())) {
      logger.info("Succesfully connected")
      networks(network.name())
    } else if (maxTry > 0) {
      blocking(Thread.sleep(1000))
      logger.info(s"$errMsg, retrying. Max tries = $maxTry")
      waitForNetwork(containerId, maxTry - 1)
    } else {
      throw new IllegalStateException(errMsg)
    }
  }

  def startNodes(nodeConfig: Seq[Config]): List[Node] = {
    logger.info(s"Starting ${nodeConfig.size} containers")
    val nodes: List[Node] = nodeConfig.map(cfg => startNodeInternal(generateConfigWithCorrectKnownPeers(cfg))).toList
    blocking(Thread.sleep(nodeConfig.size * 5000))
    nodes
  }

  def generateConfigWithCorrectKnownPeers(nodeConfig: Config): Config = {
    val config = nodeConfig
      .withFallback(Configs.knownPeers(nodes.asScala.map(node => node.nodeIp -> 9001).toSeq))
      .withFallback(defaultConf)
      .resolve()
    config
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

  private def endpointConfigFor(ip: String): EndpointConfig = {
    EndpointConfig.builder()
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

  private def buildPeerContainerConfig(nodeConfig: Config,
                                       settings: EncryAppSettings,
                                       ip: String,
                                       specialVolumeOpt: Option[(String, String)] = None): ContainerConfig = {
    val restApiPort = settings.restApi.bindAddress.getPort
    val networkPort = settings.network.bindAddress.getPort
    val portBindings = new ImmutableMap.Builder[String, JList[PortBinding]]()
      .put(restApiPort.toString, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .put(networkPort.toString, Collections.singletonList(PortBinding.randomPort("0.0.0.0")))
      .build()

    val hostConfig = specialVolumeOpt
      .map { case (lv, rv) =>
        HostConfig.builder()
          .appendBinds(s"$lv:$rv")
      }
      .getOrElse(HostConfig.builder())
      .portBindings(portBindings)
      .memory(4294967296L) //limit memory to 4G
      .build()

    val networkingConfig = ContainerConfig.NetworkingConfig
      .create(Map(networkName -> endpointConfigFor(ip)).asJava)

    val configCommandLine = renderProperties(asProperties(nodeConfig))

    ContainerConfig.builder()
      .image("it/it")
      .exposedPorts(restApiPort.toString, networkPort.toString)
      .networkingConfig(networkingConfig)
      .hostConfig(hostConfig)
      .env(s"OPTS=$configCommandLine")
      .build()
  }

  def startNodeInternal(nodeConfig: Config, specialVolumeOpt: Option[(String, String)] = None): Node =
    try {
      val settings = EncryAppSettings.fromConfig(nodeConfig.withFallback(configTemplate))
      val nodeNumber = settings.network.nodeName.map(_.replace("node", "").toInt).getOrElse(0)
      val ip = ipForNode(nodeNumber)

      val containerConfig = buildPeerContainerConfig(nodeConfig, EncryAppSettings.fromConfig(nodeConfig), ip, specialVolumeOpt)

      val containerId = {
        val containerName = networkName + "-" + settings.network.nodeName.getOrElse("NodeWithoutName") + "-" + uuidShort
        dumpContainers(
          client.listContainers(DockerClient.ListContainersParam.filter("name", containerName)),
          "Containers with same name"
        )

        val r = client.createContainer(containerConfig, containerName)
        Option(r.warnings().asScala).toSeq.flatten.foreach(logger.warn(_))
        r.id()
      }
      val attachedNetwork = connectToNetwork(containerId, ip)
      client.startContainer(containerId)
      val containerInfo = client.inspectContainer(containerId)
      val ports = containerInfo.networkSettings().ports()
      val hostPort = extractHostPort(ports, extractNetworkPortFromConfig(nodeConfig).getOrElse(9001))
      val hostRestApiPort = extractHostPort(ports,  extractApiPortFromConfig(nodeConfig).getOrElse(9051))
      val node = new Node(nodeConfig, hostRestApiPort, containerId, attachedNetwork.ipAddress(), hostPort, http)
      nodes.add(node)
      logger.debug(s"Started $containerId -> ${node.name}")
      node
    } catch {
      case NonFatal(e) =>
        logger.error("Can't start a container", e)
        dumpContainers(client.listContainers())
        throw e
    }

  def stopNode(node: Node, secondsToWaitBeforeKilling: Int = 0) {
    client.stopContainer(node.containerId, secondsToWaitBeforeKilling)
    client.removeContainer(node.containerId, RemoveContainerParam.forceKill())
  }

  def extractHostPort(portBindingMap: JMap[String, JList[PortBinding]], containerPort: Int): Int =
    portBindingMap.get(s"$containerPort/tcp").get(0).hostPort().toInt

  def extractNetworkPortFromConfig(config: Config): Option[Int] =
    Try(config.getString("encry.network.bindAddress").split(":")(1).toInt).toOption

  def extractApiPortFromConfig(config: Config): Option[Int] =
    Try(config.getString("encry.restApi.bindAddress").split(":")(1).toInt).toOption

  private def saveNodeLogs(): Unit = {
    val logDir = Paths.get(System.getProperty("user.dir"), "target", "logs")
    Files.createDirectories(logDir)
    nodes.asScala.foreach { node =>

      val fileName = if (tag.isEmpty) node.containerId else s"$tag-${node.containerId}"
      val logFile = logDir.resolve(s"$fileName.log").toFile
      logger.info(s"Writing logs of ${node.containerId} to ${logFile.getAbsolutePath}")

      val fileStream = new FileOutputStream(logFile, false)
      client.logs(
        node.containerId,
        DockerClient.LogsParam.timestamps(),
        DockerClient.LogsParam.follow(),
        DockerClient.LogsParam.stdout(),
        DockerClient.LogsParam.stderr()
      )
        .attach(fileStream, fileStream)
    }
  }

  override def close(): Unit = {
    if (isStopped.compareAndSet(false, true)) {
      logger.info("Stopping containers")
      nodes.asScala.foreach { node =>
        node.close()
        client.stopContainer(node.containerId, 0)
      }
      saveNodeLogs()
      nodes.asScala.foreach(_.client.close())

      nodes.asScala foreach { node =>
        client.removeContainer(node.containerId, RemoveContainerParam.forceKill())
      }
      client.removeNetwork(network.id())
      client.close()
    }
  }
}

object Docker {
  private val jsonMapper: ObjectMapper = new ObjectMapper
  private val propsMapper: JavaPropsMapper = new JavaPropsMapper
  val networkNamePrefix: String = "itest-"
  val configTemplate: Config = parseResources("template.conf")

  val defaultConf: Config = ConfigFactory.load

  def asProperties(config: Config): Properties = {
    val jsonConfig = config.getObject("encry").render(ConfigRenderOptions.concise())
    propsMapper.writeValueAsProperties(jsonMapper.readTree(jsonConfig))
  }

  def renderProperties(p: Properties): String =
    p.asScala
      .map {
        case (k, v) if v.contains(" ") => k -> s"""\"$v\""""
        case x                         => x
      }
      .map { case (k, v) => s"-Dencry.$k=$v" }
      .mkString(" ")

  def apply(owner: Class[_]): Docker = new Docker(tag = owner.getSimpleName)

  case class NodeInfo(nodeApiEndpoint: URL,
                      hostNetworkAddress: InetSocketAddress,
                      containerNetworkAddress: InetSocketAddress)
}

