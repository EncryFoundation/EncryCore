package encry.it.docker

import java.io.{FileOutputStream, IOException}
import java.net.{InetAddress, InetSocketAddress, URL}
import java.nio.file.{Files, Path, Paths}
import java.util.Collections._
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean
import java.util.{Properties, List => JList, Map => JMap}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.javaprop.JavaPropsMapper
import com.google.common.collect.ImmutableMap
import com.google.common.primitives.Ints._
import com.spotify.docker.client.messages.EndpointConfig.EndpointIpamConfig
import com.spotify.docker.client.messages._
import com.spotify.docker.client.{DefaultDockerClient, DockerClient}
import com.typesafe.config.ConfigFactory._
import com.typesafe.config.{Config, ConfigRenderOptions}
import monix.eval.Coeval
import encry.it.util.GlobalTimer.{timer => timer}
import encry.settings.EncryAppSettings
import encry.utils.Logging
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.apache.commons.compress.archivers.ArchiveStreamFactory
import org.apache.commons.compress.archivers.tar.TarArchiveEntry
import org.apache.commons.io.IOUtils
import org.asynchttpclient.Dsl._

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, blocking}
import scala.util.control.NonFatal
import scala.util.{Random, Try}
import encry.it.api.AsyncHttpApi._

class Docker(suiteConfig: Config = empty,
             tag: String = "",
             enableProfiling: Boolean = false) extends AutoCloseable with Logging {

  import Docker._

  private val http = asyncHttpClient(
    config()
      .setNettyTimer(timer)
      .setMaxConnections(18)
      .setMaxConnectionsPerHost(3)
      .setMaxRequestRetry(1)
      .setReadTimeout(10000)
      .setKeepAlive(false)
      .setRequestTimeout(10000))

  private val client = DefaultDockerClient.fromEnv().build()

  private val nodes     = ConcurrentHashMap.newKeySet[DockerNode]()
  private val isStopped = new AtomicBoolean(false)

  dumpContainers(client.listContainers())
  sys.addShutdownHook {
    logDebug("Shutdown hook")
    close()
  }

  private val genesisOverride = Docker.genesisOverride

  // a random network in 10.x.x.x range
  private val networkSeed = Random.nextInt(0x100000) << 4 | 0x0A000000
  // 10.x.x.x/28 network will accommodate up to 13 nodes
  private val networkPrefix = s"${InetAddress.getByAddress(toByteArray(networkSeed)).getHostAddress}/28"

  private val logDir: Coeval[Path] = Coeval.evalOnce {
    val r = Option(System.getProperty("encry.it.logging.dir"))
      .map(Paths.get(_))
      .getOrElse(Paths.get(System.getProperty("user.dir"), "target", "logs"))

    Files.createDirectories(r)
    r
  }

  private def ipForNode(nodeId: Int) = InetAddress.getByAddress(toByteArray(nodeId & 0xF | networkSeed)).getHostAddress

  private lazy val network: Network = {
    val networkName = s"encry-${hashCode().toLong.toHexString}"

    def network: Option[Network] =
      try {
        val networks = client.listNetworks(DockerClient.ListNetworksParam.byNetworkName(networkName))
        if (networks.isEmpty) None else Some(networks.get(0))
      } catch {
        case NonFatal(_) => network
      }

    def attempt(rest: Int): Network =
      try {
        network match {
          case Some(n) =>
            val ipam = s"ipam: ${n
              .ipam()
              .config()
              .asScala
              .map { n =>
                s"subnet=${n.subnet()}, ip range=${n.ipRange()}"
              }
              .mkString(", ")}"
            logInfo(s"Network ${n.name()} (id: ${n.id()}) is created for $tag, $ipam")
            n
          case None =>
            logDebug(s"Creating network $networkName for $tag")
            // Specify the network manually because of race conditions: https://github.com/moby/moby/issues/20648
            val r = client.createNetwork(
              NetworkConfig
                .builder()
                .name(networkName)
                .ipam(
                  Ipam
                    .builder()
                    .driver("default")
                    .config(singletonList(IpamConfig.create(networkPrefix, networkPrefix, ipForNode(0xE))))
                    .build()
                )
                .checkDuplicate(true)
                .build())
            Option(r.warnings()).foreach(logWarn)
            attempt(rest - 1)
        }
      } catch {
        case NonFatal(e) =>
          logWarn(s"Can not create a network for $tag. Error: $e")
          if (rest == 0) throw e else attempt(rest - 1)
      }

    attempt(5)
  }

  def startNodes(nodeConfigs: Seq[Config]): Seq[DockerNode] = {
    logInfo(s"Starting ${nodeConfigs.size} containers")
    val all = nodeConfigs.map(startNodeInternal)
    Await.result(
      for {
        _ <- Future.traverse(all)(_.waitForStartup())
        _ <- Future.traverse(all)(connectToAll)
      } yield (),
      5.minutes
    )
    all
  }

  def startNode(nodeConfig: Config, autoConnect: Boolean = true): DockerNode = {
    val node = startNodeInternal(nodeConfig)
    Await.result(
      node.waitForStartup().flatMap(_ => if (autoConnect) connectToAll(node) else Future.successful(())),
      3.minutes
    )
    node
  }

  private def connectToAll(node: DockerNode): Future[Unit] = {
    def connectToOne(address: InetSocketAddress): Future[Unit] = {
      for {
        _              <- node.connect(address)
        _              <- Future(blocking(Thread.sleep(1.seconds.toMillis)))
        connectedPeers <- node.connectedPeers
        _ <- {
          val connectedAddresses = connectedPeers.map(_.address.replaceAll("""^.*/([\d\.]+).+$""", "$1")).sorted
          logDebug(s"Looking for ${address.getHostName} in $connectedAddresses")
          if (connectedAddresses.contains(address.getHostName)) Future.successful(())
          else {
            logDebug(s"Not found ${address.getHostName}, retrying")
            connectToOne(address)
          }
        }
      } yield ()
    }

    val seedAddresses = nodes.asScala
      .filterNot(_.name == node.name)
      .filterNot { node =>
        // Exclude disconnected
        client.inspectContainer(node.containerId).networkSettings().networks().isEmpty
      }
      .map(_.containerNetworkAddress)

    if (seedAddresses.isEmpty) Future.successful(())
    else
      Future
        .traverse(seedAddresses)(connectToOne)
        .map(_ => ())
  }

  private def startNodeInternal(nodeConfig: Config): DockerNode =
    try {
      val overrides = nodeConfig
        .withFallback(suiteConfig)
        .withFallback(genesisOverride)

      val actualConfig = overrides
        .withFallback(configTemplate)
        .withFallback(defaultApplication())
        .withFallback(defaultReference())
        .resolve()

      val restApiPort    = actualConfig.getString("encry.rest-api.port")
      val networkPort    = actualConfig.getString("encry.network.port")
      val matcherApiPort = actualConfig.getString("encry.matcher.port")

      val portBindings = new ImmutableMap.Builder[String, java.util.List[PortBinding]]()
        .put(s"$ProfilerPort", singletonList(PortBinding.randomPort("0.0.0.0")))
        .put(restApiPort, singletonList(PortBinding.randomPort("0.0.0.0")))
        .put(networkPort, singletonList(PortBinding.randomPort("0.0.0.0")))
        .put(matcherApiPort, singletonList(PortBinding.randomPort("0.0.0.0")))
        .build()

      val hostConfig = HostConfig
        .builder()
        .portBindings(portBindings)
        .build()

      val nodeName   = actualConfig.getString("encry.network.node-name")
      val nodeNumber = nodeName.replace("node", "").toInt
      val ip         = ipForNode(nodeNumber)

      val javaOptions = Option(System.getenv("CONTAINER_JAVA_OPTS")).getOrElse("")
      val configOverrides: String = {
        val ntpServer = Option(System.getenv("NTP_SERVER")).fold("")(x => s"-Dencry.ntp-server=$x ")

        var config = s"$javaOptions ${renderProperties(asProperties(overrides))} " +
          s"-Dlogback.stdout.level=TRACE -Dlogback.file.level=OFF -Dencry.network.declared-address=$ip:$networkPort $ntpServer"

        val withAspectJ = Option(System.getenv("WITH_ASPECTJ")).fold(false)(_.toBoolean)
        if (withAspectJ) config += s"-javaagent:$ContainerRoot/aspectjweaver.jar "
        config
      }

      val containerConfig = ContainerConfig
        .builder()
        .image("bromel777/encry-core:testContainer")
        .exposedPorts(s"$ProfilerPort", restApiPort, networkPort, matcherApiPort)
        .networkingConfig(ContainerConfig.NetworkingConfig.create(Map(
          network.name() -> endpointConfigFor(nodeName)
        ).asJava))
        .hostConfig(hostConfig)
        .build()

      val containerId = {
        val containerName = s"${network.name()}-$nodeName"
        dumpContainers(
          client.listContainers(DockerClient.ListContainersParam.filter("name", containerName)),
          "Containers with same name"
        )

        logDebug(s"Creating container $containerName at $ip with options: $javaOptions")
        val r = client.createContainer(containerConfig, containerName)
        Option(r.warnings().asScala).toSeq.flatten.foreach(logWarn)
        r.id()
      }

      client.startContainer(containerId)

      val node = new DockerNode(actualConfig,
        containerId,
        getNodeInfo(containerId, EncryAppSettings.fromConfig(actualConfig)))
      nodes.add(node)
      logDebug(s"Started $containerId -> ${node.name}: ${node.nodeInfo}")
      node
    } catch {
      case NonFatal(e) =>
        logError(s"Can't start a container. Error: $e")
        dumpContainers(client.listContainers())
        throw e
    }

  private def getNodeInfo(containerId: String, settings: EncryAppSettings): NodeInfo = {
    val restApiPort    = settings.restApi.bindAddress.getPort
    val networkPort    = settings.network.bindAddress.getPort

    val containerInfo = inspectContainer(containerId)
    val ports         = containerInfo.networkSettings().ports()

    val ipAddress = containerInfo.networkSettings().networks().get(network.name()).ipAddress()

    NodeInfo(
      new URL(s"http://localhost:${extractHostPort(ports, restApiPort)}"),
      new InetSocketAddress("localhost", extractHostPort(ports, networkPort)),
      new InetSocketAddress(ipAddress, networkPort)
    )
  }

  private def inspectContainer(containerId: String): ContainerInfo = {
    val containerInfo = client.inspectContainer(containerId)
    if (containerInfo.networkSettings().networks().asScala.contains(network.name())) containerInfo
    else {
      logDebug(s"Container $containerId has not connected to the network ${network.name()} yet, retry")
      Thread.sleep(1000)
      inspectContainer(containerId)
    }
  }

  def stopContainer(node: DockerNode): Unit = {
    val id = node.containerId
    logInfo(s"Stopping container with id: $id")
    takeProfileSnapshot(node)
    client.stopContainer(node.containerId, 10)
    saveProfile(node)
    saveLog(node)
    val containerInfo = client.inspectContainer(node.containerId)
    logDebug(s"""Container information for ${node.name}:
                 |Exit code: ${containerInfo.state().exitCode()}
                 |Error: ${containerInfo.state().error()}
                 |Status: ${containerInfo.state().status()}
                 |OOM killed: ${containerInfo.state().oomKilled()}""".stripMargin)
  }

  def killAndStartContainer(node: DockerNode): DockerNode = {
    val id = node.containerId
    logInfo(s"Killing container with id: $id")
    takeProfileSnapshot(node)
    client.killContainer(id, DockerClient.Signal.SIGINT)
    saveProfile(node)
    saveLog(node)
    client.startContainer(id)
    node.nodeInfo = getNodeInfo(node.containerId, node.settings)
    Await.result(
      node.waitForStartup().flatMap(_ => connectToAll(node)),
      3.minutes
    )
    node
  }

  def restartNode(node: DockerNode, configUpdates: Config = empty): DockerNode = {
    Await.result(node.waitForHeightArise, 3.minutes)

    if (configUpdates != empty) {
      val renderedConfig = renderProperties(asProperties(configUpdates))

      logDebug("Set new config directly in the script for starting node")
      val shPath = "/container/start-node.sh"
      val scriptCmd: Array[String] =
        Array("sh", "-c", s"sed -i 's|$$Encry_OPTS.*-jar|$$Encry_OPTS $renderedConfig -jar|' $shPath && chmod +x $shPath")

      val execScriptCmd = client.execCreate(node.containerId, scriptCmd).id()
      client.execStart(execScriptCmd)
    }

    restartContainer(node)
  }

  override def close(): Unit = {
    if (isStopped.compareAndSet(false, true)) {
      logInfo("Stopping containers")

      nodes.asScala.foreach { node =>
        takeProfileSnapshot(node)
        client.stopContainer(node.containerId, 0)

        saveProfile(node)
        saveLog(node)
        val containerInfo = client.inspectContainer(node.containerId)
        logDebug(s"""Container information for ${node.name}:
                     |Exit code: ${containerInfo.state().exitCode()}
                     |Error: ${containerInfo.state().error()}
                     |Status: ${containerInfo.state().status()}
                     |OOM killed: ${containerInfo.state().oomKilled()}""".stripMargin)

        try {
          client.removeContainer(node.containerId)
        } catch {
          case NonFatal(e) => logWarn(s"Can't remove a container of ${node.name}", e)
        }
      }

      try {
        client.removeNetwork(network.id)
      } catch {
        case NonFatal(e) =>
          // https://github.com/moby/moby/issues/17217
          logWarn(s"Can not remove network ${network.name()}", e)
      }

      http.close()
      client.close()
    }
  }

  private def saveLog(node: DockerNode): Unit = {
    val containerId = node.containerId
    val logFile     = logDir().resolve(s"${node.name}.log").toFile
    logInfo(s"Writing logs of $containerId to ${logFile.getAbsolutePath}")

    val fileStream = new FileOutputStream(logFile, false)
    try {
      client
        .logs(
          containerId,
          DockerClient.LogsParam.follow(),
          DockerClient.LogsParam.stdout(),
          DockerClient.LogsParam.stderr()
        )
        .attach(fileStream, fileStream)
    } finally {
      fileStream.close()
    }
  }

  private def takeProfileSnapshot(node: DockerNode): Unit = if (enableProfiling) {
    val task = client.execCreate(
      node.containerId,
      Array(
        "java",
        "-jar",
        ProfilerController.toString,
        "127.0.0.1",
        ProfilerPort.toString,
        "capture-performance-snapshot"
      ),
      DockerClient.ExecCreateParam.attachStdout(),
      DockerClient.ExecCreateParam.attachStderr()
    )
    Option(task.warnings()).toSeq.flatMap(_.asScala).foreach(logWarn(_))
    client.execStart(task.id())
    while (client.execInspect(task.id()).running()) {
      logInfo(s"Snapshot of ${node.name} has not been took yet, wait...")
      blocking(Thread.sleep(1000))
    }
  }

  private def saveProfile(node: DockerNode): Unit = if (enableProfiling) {
    try {
      val profilerDirStream = client.archiveContainer(node.containerId, ContainerRoot.resolve("profiler").toString)

      try {
        val archiveStream = new ArchiveStreamFactory().createArchiveInputStream(ArchiveStreamFactory.TAR, profilerDirStream)
        val snapshotFile = Iterator
          .continually(Option(archiveStream.getNextEntry))
          .takeWhile(_.nonEmpty)
          .collectFirst {
            case Some(entry: TarArchiveEntry) if entry.isFile && entry.getName.contains(".snapshot") => entry
          }

        snapshotFile.foreach { archiveFile =>
          val output = new FileOutputStream(logDir().resolve(s"${node.name}.snapshot").toFile)
          try {
            IOUtils.copy(archiveStream, output)
            logInfo(s"The snapshot of ${node.name} was successfully saved")
          } catch {
            case e: Throwable => throw new IOException(s"Can't copy ${archiveFile.getName} of ${node.name} to local fs", e)
          } finally {
            output.close()
          }
        }
      } catch {
        case e: Throwable => throw new IOException(s"Can't read a profiler directory stream of ${node.name}", e)
      } finally {
        // Some kind of https://github.com/spotify/docker-client/issues/745
        // But we have to close this stream, otherwise the thread will be blocked
        Try(profilerDirStream.close())
      }
    } catch {
      case e: Throwable => logWarn(s"Can't save profiler logs of ${node.name}", e)
    }
  }

  def disconnectFromNetwork(node: DockerNode): Unit = disconnectFromNetwork(node.containerId)

  private def disconnectFromNetwork(containerId: String): Unit = client.disconnectFromNetwork(containerId, network.id())

  def restartContainer(node: DockerNode): DockerNode = {
    val id            = node.containerId
    val containerInfo = inspectContainer(id)
    val ports         = containerInfo.networkSettings().ports()
    logInfo(s"New ports: ${ports.toString}")
    client.restartContainer(id, 10)

    node.nodeInfo = getNodeInfo(node.containerId, node.settings)
    Await.result(
      node.waitForStartup().flatMap(_ => connectToAll(node)),
      3.minutes
    )
    node
  }

  def connectToNetwork(nodes: Seq[DockerNode]): Unit = {
    nodes.foreach(connectToNetwork)
    Await.result(Future.traverse(nodes)(connectToAll), 1.minute)
  }

  private def connectToNetwork(node: DockerNode): Unit = {
    client.connectToNetwork(
      network.id(),
      NetworkConnection
        .builder()
        .containerId(node.containerId)
        .endpointConfig(endpointConfigFor(node.name))
        .build()
    )

    node.nodeInfo = getNodeInfo(node.containerId, node.settings)
    logDebug(s"New ${node.name} settings: ${node.nodeInfo}")
  }

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

    logDebug(s"$label: $x")
  }

  def runMigrationToolInsideContainer(node: DockerNode): DockerNode = {
    val id = node.containerId
    takeProfileSnapshot(node)
    updateStartScript(node)
    stopContainer(node)
    saveProfile(node)
    saveLog(node)
    client.startContainer(id)
    client.waitContainer(id)
    client.startContainer(id)
    node.nodeInfo = getNodeInfo(node.containerId, node.settings)
    Await.result(
      node.waitForStartup().flatMap(_ => connectToAll(node)),
      3.minutes
    )
    node
  }

  private def updateStartScript(node: DockerNode): Unit = {
    val id = node.containerId

    val cpCmd: Array[String] =
      Array(
        "sh",
        "-c",
        s"""cp /container/start-node.sh"""
      )
    val execCpCmd = client.execCreate(id, cpCmd).id()
    client.execStart(execCpCmd)

    logDebug("Change script for migration tool launch")
    val scriptCmd: Array[String] = Array()
      //script for node
    val execScriptCmd = client.execCreate(id, scriptCmd).id()
    client.execStart(execScriptCmd)
  }

}

object Docker {
  private val ContainerRoot      = Paths.get("/opt/encry")
  private val ProfilerController = ContainerRoot.resolve("yjp-controller-api-redist.jar")
  private val ProfilerPort       = 10001
  private val jsonMapper         = new ObjectMapper
  private val propsMapper        = new JavaPropsMapper

  val configTemplate = parseResources("template.conf")
  def genesisOverride: Config = {
    val genesisTs          = System.currentTimeMillis()

    val genesisConfig    = configTemplate.withFallback(timestampOverrides)
    val genesisSignature = Block.genesis(gs).explicitGet().uniqueId

  }

  AddressScheme.current = new AddressScheme {
  }

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

  class DockerNode(config: Config, val containerId: String, private[Docker] var nodeInfo: NodeInfo) extends Node(config) {
    override def nodeApiEndpoint: URL = nodeInfo.nodeApiEndpoint

    override val apiKey = "integration-test-rest-api"

    override def networkAddress: InetSocketAddress = nodeInfo.hostNetworkAddress

    def containerNetworkAddress: InetSocketAddress = nodeInfo.containerNetworkAddress

    def getConfig: Config = config
  }

}

