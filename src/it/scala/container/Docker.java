package container;

class Docker(suiteConfig: Config = ConfigFactory.empty,
             tag: String = "",
             localDataVolumeOpt: Option[String] = None)
            (implicit ec: ExecutionContext) extends AutoCloseable with StrictLogging {

  import Docker._

  private val http = asyncHttpClient(config()
    .setMaxConnections(50)
    .setMaxConnectionsPerHost(10)
    .setMaxRequestRetry(1)
    .setReadTimeout(10000)
    .setRequestTimeout(10000))

  private val client = DefaultDockerClient.fromEnv().build()
  private var nodeRepository = Seq.empty[Node]
  private var apiCheckerOpt: Option[ApiChecker] = None
  private val isStopped = new AtomicBoolean(false)

  // This should be called after client is ready but before network created.
  // This allows resource cleanup for the network if we are running out of them
  initBeforeStart()

  private def uuidShort: String = UUID.randomUUID().hashCode().toHexString
  private val networkName = Docker.networkNamePrefix + uuidShort
  private val networkSeed = Random.nextInt(0x100000) << 4 | 0x0A000000
  private val networkPrefix = s"${InetAddress.getByAddress(Ints.toByteArray(networkSeed)).getHostAddress}/28"
  private val innerNetwork: Network = createNetwork(3)

  def nodes: Seq[Node] = nodeRepository

  def initBeforeStart(): Unit = {
    cleanupDanglingIfNeeded()
    sys.addShutdownHook {
      close()
    }
  }

  def startNodes(nodeConfigs: List[Config],
                 configEnrich: ExtraConfig = noExtraConfig): Try[List[Node]] = {
    logger.trace(s"Starting ${nodeConfigs.size} containers")
    val nodes: Try[List[Node]] = nodeConfigs.map(cfg => startNode(cfg, configEnrich)).sequence
    blocking(Thread.sleep(nodeConfigs.size * 5000))
    nodes
  }

  def waitForStartupBlocking(nodes: List[Node]): List[Node] = {
    logger.debug("Waiting for nodes to start")
    Await.result(waitForStartup(nodes), nodes.size * 90.seconds)
  }

  def waitForStartup(nodes: List[Node]): Future[List[Node]] = {
    Future.sequence(nodes map { _.waitForStartup })
  }

  def waitContainer(id: String): ContainerExit = client.waitContainer(id)

  def startOpenApiChecker(checkerInfo: ApiCheckerConfig): Try[ApiChecker] = Try {
    client.pull(ApiCheckerImageStable)

    val ip: String = ipForNode(999, networkSeed)
    val containerId: String = client.createContainer(buildApiCheckerContainerConfig(checkerInfo, ip)).id
    connectToNetwork(containerId, ip)
    client.startContainer(containerId)

    logger.info(s"Started ApiChecker: $containerId")

    val checker: ApiChecker = ApiChecker(containerId, checkerInfo)
    apiCheckerOpt = Some(checker)
    checker
  }

  def startNode(nodeSpecificConfig: Config,
                extraConfig: ExtraConfig = noExtraConfig,
                specialVolumeOpt: Option[(String, String)] = None): Try[Node] = {
    val initialSettings = buildSettings(nodeSpecificConfig)
    val configuredNodeName = initialSettings.scorexSettings.network.nodeName
    val nodeNumber = configuredNodeName.replace("node", "").toInt
    val ip = ipForNode(nodeNumber, networkSeed)
    val restApiPort = initialSettings.scorexSettings.restApi.bindAddress.getPort
    val networkPort = initialSettings.scorexSettings.network.bindAddress.getPort

    val nodeConfig: Config = enrichNodeConfig(nodeSpecificConfig, extraConfig, ip, networkPort)
    val settings: Settings = buildSettings(nodeConfig)
    val containerConfig: ContainerConfig = buildPeerContainerConfig(nodeConfig, settings, ip, specialVolumeOpt)
    val containerName = networkName + "-" + configuredNodeName + "-" + uuidShort

    Try {
      val containerId = client.createContainer(containerConfig, containerName).id
      val attachedNetwork = connectToNetwork(containerId, ip)
      client.startContainer(containerId)

      val containerInfo = client.inspectContainer(containerId)
      val ports = containerInfo.networkSettings().ports()

      val nodeInfo = NodeInfo(
        hostRestApiPort = extractHostPort(ports, restApiPort),
        hostNetworkPort = extractHostPort(ports, networkPort),
        containerNetworkPort = networkPort,
        containerApiPort = restApiPort,
        apiIpAddress = containerInfo.networkSettings().ipAddress(),
        networkIpAddress = attachedNetwork.ipAddress(),
        containerId = containerId)

      logger.info(s"Started node: $nodeInfo")

      val node = new Node(settings, nodeInfo, http)
      nodeRepository = nodeRepository :+ node
      node
    } recoverWith {
      case e: ImageNotFoundException =>
        Failure(new Exception(s"Error: docker image is missing. Run 'sbt it:test' to generate it.", e))
    }
  }

  private def buildSettings(nodeConfig: Config) = {
    val actualConfig = nodeConfig
      .withFallback(suiteConfig)
      .withFallback(defaultConfigTemplate)
      .withFallback(ConfigFactory.defaultApplication())
      .withFallback(ConfigFactory.defaultReference())
      .resolve()
    Settings.fromConfig(actualConfig)
  }

  private def enrichNodeConfig(nodeConfig: Config, extraConfig: ExtraConfig, ip: String, port: Int) = {
    val publicPeerConfig = nodeConfig//.withFallback(declaredAddressConfig(ip, port))
    val withPeerConfig = nodeRepository.headOption.fold(publicPeerConfig) { node =>
      knownPeersConfig(Seq(node.nodeInfo)).withFallback(publicPeerConfig)
    }
    val enrichedConfig = extraConfig(this, nodeConfig).fold(withPeerConfig)(_.withFallback(withPeerConfig))
    val actualConfig = enrichedConfig.withFallback(suiteConfig).withFallback(defaultConfigTemplate)
    actualConfig
  }

  private def buildApiCheckerContainerConfig(checkerInfo: ApiCheckerConfig, ip: String): ContainerConfig = {
    val hostConfig: HostConfig = HostConfig.builder()
      .appendBinds(s"${checkerInfo.specFilePath}:/app/openapi.yaml", s"${checkerInfo.paramsFilePath}:/app/parameters.yaml")
      .build()

    val networkingConfig: ContainerConfig.NetworkingConfig = ContainerConfig.NetworkingConfig
      .create(Map(networkName -> endpointConfigFor(ip)).asJava)

    ContainerConfig.builder()
      .image(ApiCheckerImageStable)
      .cmd("openapi.yaml", "--api", s"http://${checkerInfo.apiAddressToCheck}", "--parameters", "parameters.yaml")
      .networkingConfig(networkingConfig)
      .hostConfig(hostConfig)
      .build()
  }

  private def buildPeerContainerConfig(nodeConfig: Config,
                                       settings: Settings,
                                       ip: String,
                                       specialVolumeOpt: Option[(String, String)] = None): ContainerConfig = {
    val restApiPort = settings.scorexSettings.restApi.bindAddress.getPort
    val networkPort = settings.scorexSettings.network.bindAddress.getPort
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
      .memory(1L << 30) //limit memory to 1G
      .build()

    val networkingConfig = ContainerConfig.NetworkingConfig
      .create(Map(networkName -> endpointConfigFor(ip)).asJava)

    val configCommandLine = renderProperties(asProperties(nodeConfig))

    ContainerConfig.builder()
      .image(ImageLatest)
      .exposedPorts(restApiPort.toString, networkPort.toString)
      .networkingConfig(networkingConfig)
      .hostConfig(hostConfig)
      .env(s"OPTS=$configCommandLine")
      .build()
  }

  private def createNetwork(maxRetry: Int = 5): Network = try {
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
    val config = IpamConfig.create(networkPrefix, networkPrefix, ipForNode(0xE, networkSeed))
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

  private def connectToNetwork(containerId: String, ip: String): AttachedNetwork = {
    client.connectToNetwork(
      innerNetwork.id(),
      NetworkConnection
        .builder()
        .containerId(containerId)
        .endpointConfig(endpointConfigFor(ip))
        .build()
    )
    waitForNetwork(containerId)
  }

  @tailrec private def waitForNetwork(containerId: String, maxTry: Int = 5): AttachedNetwork = {
    def errMsg = s"Container $containerId has not connected to the network ${innerNetwork.name()}"
    val containerInfo = client.inspectContainer(containerId)
    val networks = containerInfo.networkSettings().networks().asScala
    if (networks.contains(innerNetwork.name())) {
      networks(innerNetwork.name())
    } else if (maxTry > 0) {
      blocking(Thread.sleep(1000))
      logger.debug(s"$errMsg, retrying. Max tries = $maxTry")
      waitForNetwork(containerId, maxTry - 1)
    } else {
      throw new IllegalStateException(errMsg)
    }
  }

  def stopNode(containerId: String, secondsToWait: Int = 5): Unit = {
    nodeRepository = nodeRepository.filterNot(_.containerId == containerId)
    client.stopContainer(containerId, secondsToWait)
  }

  def forceStopNode(containerId: String): Unit = {
    nodeRepository = nodeRepository.filterNot(_.containerId == containerId)
    client.removeContainer(containerId, RemoveContainerParam.forceKill())
  }

  override def close(): Unit = {
    if (isStopped.compareAndSet(false, true)) {
      logger.info("Stopping containers")
      nodeRepository foreach { node =>
        node.close()
        client.stopContainer(node.containerId, 0)
      }
      http.close()

      saveNodeLogs()

      apiCheckerOpt.foreach { checker =>
        saveLogs(checker.containerId, "openapi-checker")
        client.removeContainer(checker.containerId, RemoveContainerParam.forceKill())
      }

      nodeRepository foreach { node =>
        client.removeContainer(node.containerId, RemoveContainerParam.forceKill())
      }
      client.removeNetwork(innerNetwork.id())
      client.close()

      localDataVolumeOpt.foreach { path =>
        val dataVolume = new File(path)
        FileUtils.deleteDirectory(dataVolume)
      }
    }
  }

  private def saveLogs(containerId: String, tag: String): Unit = {
    val logDir: Path = Paths.get(System.getProperty("user.dir"), "target", "logs")
    Files.createDirectories(logDir)

    val fileName: String = s"$tag-$containerId"
    val logFile: File = logDir.resolve(s"$fileName.log").toFile
    logger.info(s"Writing logs of $tag-$containerId to ${logFile.getAbsolutePath}")

    val fileStream: FileOutputStream = new FileOutputStream(logFile, false)
    client.logs(
      containerId,
      DockerClient.LogsParam.timestamps(),
      DockerClient.LogsParam.follow(),
      DockerClient.LogsParam.stdout(),
      DockerClient.LogsParam.stderr()
    )
      .attach(fileStream, fileStream)
  }

  private def saveNodeLogs(): Unit = {
    val logDir = Paths.get(System.getProperty("user.dir"), "target", "logs")
    Files.createDirectories(logDir)
    nodeRepository.foreach { node =>
      import node.nodeInfo.containerId

      val fileName = if (tag.isEmpty) containerId else s"$tag-$containerId"
      val logFile = logDir.resolve(s"$fileName.log").toFile
      logger.info(s"Writing logs of $containerId to ${logFile.getAbsolutePath}")

      val fileStream = new FileOutputStream(logFile, false)
      client.logs(
        containerId,
        DockerClient.LogsParam.timestamps(),
        DockerClient.LogsParam.follow(),
        DockerClient.LogsParam.stdout(),
        DockerClient.LogsParam.stderr()
      )
        .attach(fileStream, fileStream)
    }
  }

  def disconnectFromNetwork(containerId: String): Unit = client.disconnectFromNetwork(containerId, innerNetwork.id())

  def disconnectFromNetwork(node: Node): Unit = disconnectFromNetwork(node.nodeInfo.containerId)

  def connectToNetwork(node: Node): Unit = connectToNetwork(node.nodeInfo.containerId, node.nodeInfo.networkIpAddress)

  def cleanupDanglingResources(): Unit = {
    logger.debug("Cleaning up Docker resources")

    // remove containers
    client.listContainers(ListContainersParam.allContainers()).asScala
      .filter(_.names.asScala.head.startsWith("/" + networkNamePrefix))
      .foreach(c => client.removeContainer(c.id, RemoveContainerParam.forceKill))

    // removes networks
    client.listNetworks(ListNetworksParam.customNetworks).asScala
      .filter(_.name().startsWith(networkNamePrefix))
      .foreach(n => client.removeNetwork(n.id))

    //remove images
    client.listImages(ListImagesParam.danglingImages()).asScala
      .filter(img => Option(img.labels()).exists(_.containsKey(dockerImageLabel)))
      .foreach(img => client.removeImage(img.id()))
  }

  def cleanupDanglingIfNeeded(): Unit = {
    val shouldCleanup = nodesJointConfig.getOrElse[Boolean]("testing.integration.cleanupDocker", false)
    if (shouldCleanup) {
      cleanupDanglingResources()
    }
  }
}
