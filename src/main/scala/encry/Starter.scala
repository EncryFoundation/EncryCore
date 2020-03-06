package encry

import java.net.InetSocketAddress

import akka.actor.{ Actor, ActorRef }
import akka.http.scaladsl.Http
import cats.Functor
import cats.data.{ NonEmptyChain, Validated }
import cats.instances.future._
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.validated._
import encry.Starter.InitNodeResult
import encry.api.http.DataHolderForApi
import encry.api.http.DataHolderForApi.PassForStorage
import encry.cli.ConsoleListener
import encry.cli.ConsoleListener.{ prompt, StartListening }
import encry.local.miner.Miner
import encry.local.miner.Miner.StartMining
import encry.network.NetworkRouter
import encry.nvg.IntermediaryNVH
import encry.settings._
import encry.stats.StatsSender
import encry.utils.{ Mnemonic, NetworkTimeProvider }
import encry.view.mempool.MemoryPool
import encry.view.wallet.AccountManager

import scala.concurrent.Future
import scala.io.StdIn
import scala.util.{ Failure, Success, Try }

class Starter(settings: EncryAppSettings,
              timeProvider: NetworkTimeProvider,
              nodeId: Array[Byte],
              isStateExists: Boolean,
              conf: Option[String])
    extends Actor {

  import context.dispatcher

  var initHttpApiServer: Option[Future[Http.ServerBinding]] = none

  val preview: String =
    """
      |XXXXXX  XX      XX    XXXXX  XXXXXX   XX    XX
      |XX      XXXX    XX  XX       XX   XX   XXXXXX
      |XXXXXX  XX  XX  XX  XX       XXXXXX      XX
      |XX      XX    XXXX  XX       XX   XX     XX
      |XXXXXX  XX      XX    XXXXX  XX   XX     XX
    """.stripMargin

  def startNode(): Unit =
    (for {
      result <- if (conf.isEmpty && !isStateExists)
                 startEmptyNode
               else if (conf.nonEmpty && !isStateExists)
                 startFromConf
               else
                 startNonEmptyNode
    } yield result) match {
      case Left(_) =>
        println(s"Node is started with http api.")
      case Right(res) =>
        println("Configuration set up successfully!")
        self ! res
    }

  def startNonEmptyNode: Either[Throwable, InitNodeResult] =
    for {
      walletPassword <- {
        println("Please, enter wallet password:")
        readAndValidateInput(validatePassword)
      }
    } yield
      InitNodeResult(
        "",
        walletPassword,
        settings.node.offlineGeneration,
        fastSync = false,
        settings.snapshotSettings.enableSnapshotCreation,
        settings.network.knownPeers,
        settings.network.connectOnlyWithKnownPeers.getOrElse(false),
        "",
        settings.network.nodeName.getOrElse(""),
        settings.network.declaredAddress,
        settings.network.bindAddress
      )

  def startEmptyNode: Either[Throwable, InitNodeResult] =
    for {
      answer <- {
        println(preview)
        println(
          "Would you like to start node with http api helper or with cli helper? Enter 'yes' for http api or 'no' for CLI:"
        )
        readAnswer
      }
      result <- if (answer) startWithHttpApi else startWithCli
    } yield result

  def readAnswer: Either[Throwable, Boolean] = {
    def handleError: Throwable => Either[Throwable, Boolean] = (ex: Throwable) => {
      println(s"Your answer is incorrect cause $ex. Please, enter answer again")
      readAnswer
    }
    def handleResult: String => Either[Throwable, Boolean] =
      (result: String) =>
        if (result == "yes") true.asRight[Throwable]
        else if (result == "no") false.asRight[Throwable]
        else {
          println("Your answer doesn't match with yes/no answer. Please, enter it again:")
          readAnswer
      }

    Either
      .catchNonFatal(StdIn.readLine(prompt).toLowerCase)
      .fold(handleError, handleResult)
  }

  def readAndValidateInput(
    validationFunction: String => Validated[NonEmptyChain[String], String]
  ): Either[Throwable, String] = {
    def handleError: Throwable => Either[Throwable, String] = (ex: Throwable) => {
      println(s"Your input is incorrect cause $ex. Please, enter it again")
      readAndValidateInput(validationFunction)
    }
    def handleResult: String => Either[Throwable, String] = (result: String) => {
      val validatedData = validationFunction(result)
      if (validatedData.isValid) result.asRight[Throwable]
      else {
        validatedData.toEither.leftMap { errors: NonEmptyChain[String] =>
          println(s"Your input is incorrect cause: ${errors.toChain.foldLeft("") {
            case (res, nextErr) => nextErr + ". " + res
          }}")
        }
        readAndValidateInput(validationFunction)
      }
    }
    Either
      .catchNonFatal(StdIn.readLine(prompt).toLowerCase)
      .fold(handleError, handleResult)
  }

  def validatePassword(password: String): Validated[NonEmptyChain[String], String] =
    if (password.nonEmpty) password.validNec else "Password is empty".invalidNec

  def validateNodeName(nodeName: String): Validated[NonEmptyChain[String], String] =
    if (nodeName.nonEmpty) nodeName.validNec else "Node name is empty".invalidNec

  def startWithCli: Either[Throwable, InitNodeResult] = {

    def validateMnemonicKey(mnemonic: String): Validated[NonEmptyChain[String], String] = {
      val words: Array[String] = mnemonic.split(" ")
      val isValidSize: Validated[NonEmptyChain[String], String] =
        if (words.length == 12) mnemonic.validNec else "Wrong words number".invalidNec
      val isValidWords: Validated[NonEmptyChain[String], String] =
        if (words.forall(word => Mnemonic.getWords.contains(word))) mnemonic.validNec
        else "Some words don't contain in the list of available".invalidNec
      (isValidSize, isValidWords).mapN { case (_, _) => mnemonic }
    }

    def readPeersToConnect: Either[Throwable, List[InetSocketAddress]] = {
      def loop(peers: List[InetSocketAddress]): Either[Throwable, List[InetSocketAddress]] = {
        def handleError: Throwable => Either[Throwable, List[InetSocketAddress]] = (ex: Throwable) => {
          println(s"You entered incorrect input cause: ${ex.getMessage}. Enter it again")
          loop(peers)
        }
        def handleResult: InetSocketAddress => Either[Throwable, List[InetSocketAddress]] =
          (peer: InetSocketAddress) => {
            println("Would you like to enter one more peer?")
            readAnswer match {
              case Left(_)               => List.empty.asRight[Throwable]
              case Right(value) if value => loop(peers :+ peer)
              case Right(_)              => (peers :+ peer).asRight[Throwable]
            }
          }
        Either.catchNonFatal {
          println("Peer example is: '172.168.1.1:9032'. \nEnter address:")
          val addr = StdIn.readLine(prompt)
          Try {
            val split = addr.split(':')
            (split(0), split(1).toInt)
          } match {
            case Success((host, port)) =>
              new InetSocketAddress(host, port)
            case Failure(_) =>
              throw new Exception("Invalid address")
          }
        }.fold(handleError, handleResult)
      }

      loop(List.empty[InetSocketAddress])
    }

    def readDeclaredAddress(
      validationFunction: InetSocketAddress => Boolean
    ): Either[Throwable, InetSocketAddress] = {
      def handleError: Throwable => Either[Throwable, InetSocketAddress] = (ex: Throwable) => {
        println(s"You entered incorrect input cause: ${ex.getMessage}. Enter it again")
        readDeclaredAddress(validationFunction)
      }
      def handleResult: InetSocketAddress => Either[Throwable, InetSocketAddress] =
        (peer: InetSocketAddress) => { peer.asRight[Throwable] }
      Either.catchNonFatal {
        val addr = StdIn.readLine(prompt)
        Try {
          val split = addr.split(':')
          (split(0), split(1).toInt)
        } match {
          case Success((host, port)) if validationFunction(new InetSocketAddress(host, port)) =>
            new InetSocketAddress(host, port)
          case Success((_, _)) =>
            throw new Exception(
              "Declared address's port is not the same as bind address's port. " +
                "Reenter address, please!"
            )
          case Failure(_) =>
            throw new Exception("Invalid address")
        }
      }.fold(handleError, handleResult)
    }

    for {
      walletPassword <- {
        println(
          "Please, enter wallet password. Password has to be specified. \n" +
            "This password is used for keys encryption in local machine storage. \n" +
            "Password example: Nd<+IE937-pc}mYd1-Nwldnfwq-cj1s_Q%x"
        )
        readAndValidateInput(validatePassword)
      }
      mnemonicKeyAnswer <- {
        println("Would you like to enter your mnemonic key or generate a new one?")
        println("If you want to start the node with an existing mnemonic key - enter 'yes', otherwise - 'no':")
        readAnswer
      }
      mnemonicKey <- if (mnemonicKeyAnswer) {
                      println(
                        "Enter your mnemonic key. " +
                          "It has to contain 16 words and all these words have to exist in specified dictionary:"
                      )
                      readAndValidateInput(validateMnemonicKey)
                    } else {
                      val phrase: String = Mnemonic.entropyToMnemonicCode(scorex.utils.Random.randomBytes(16))
                      println(s"Your new mnemonic code is:\n'$phrase' \nPlease, save it and don't show to anyone!")
                      phrase.asRight[Throwable]
                    }
      nameNode <- {
        println(s"Enter node name:")
        readAndValidateInput(validateNodeName)
      }
      startOwnChain <- {
        println("Would you like to start your chain? If you want - enter 'yes', otherwise no:")
        readAnswer
      }
      enableFastSync <- if (startOwnChain) false.asRight[Throwable]
                       else {
                         println(
                           "Would you like to enable fast sync? This is the mod while you download blocks without \n" +
                             "validation by state and next download state with a snapshot. \n" +
                             "After finishing such process, the node is guaranteed to have a valid state and history. \n" +
                             "Enter 'yes' or 'no':"
                         )
                         readAnswer
                       }
      enableSnapshotCreation <- {
        println(s"Would you like to start snapshot creation? Enter 'yes' or 'no':")
        readAnswer
      }
      answerPeers <- {
        if (startOwnChain) false.asRight[Throwable]
        else {
          println(
            "Would you like to enter peers to connect with? \n" +
              "These are peers that you want to connect with. \n" +
              "Enter 'yes' or 'no':"
          )
          readAnswer
        }
      }
      peers <- if (answerPeers) readPeersToConnect else List.empty[InetSocketAddress].asRight[Throwable]
      connectWithOnlyKnownPeers <- {
        println(
          "Do you want to connect only with known peers? \n" +
            "This means that you allow connecting with you only to peers in your 'connect with' list. \n" +
            "Enter 'yes' or 'no':"
        )
        readAnswer
      }
      declaredAddress <- {
        println(
          "Please, set up your declared address. \n" +
            "The declared address is an address which represents the node in a network. \n" +
            "Peer example is: '172.168.1.1:9032':"
        )
        readDeclaredAddress(_ => true)
      }
      bindAddress <- {
        println(
          "Please, set your bind address. The bind address is a local machine address. \n" +
            "Bind address's port has to be the same as the declared address's port. \n" +
            "Peer example is: '0.0.0.0:9032':"
        )
        readDeclaredAddress((add: InetSocketAddress) => add.getPort == declaredAddress.getPort)
      }
    } yield
      InitNodeResult(
        mnemonicKey,
        walletPassword,
        startOwnChain,
        enableFastSync,
        enableSnapshotCreation,
        peers,
        connectWithOnlyKnownPeers,
        nodePass = "", //todo incorrect?
        nodeName = nameNode,
        declaredAddress.some,
        bindAddress
      )
  }

  def startWithHttpApi: Either[Throwable, InitNodeResult] = {
    println("Empty node will start with HTTP Api.")
    initHttpApiServer = EncryApp.configServer(self).some
    println(s"Server started at: http://0.0.0.0:9051/config")
    new Exception("Node started with http api").asLeft[InitNodeResult]
  }

  def startFromConf: Either[Throwable, InitNodeResult] = {
    println("Node will start from config")
    for {
      walletPassword <- {
        println("Please, enter wallet password:")
        readAndValidateInput(validatePassword)
      }
      nodePass <- {
        println("Please, enter node password:")
        readAndValidateInput(validatePassword)
      }
      frontName <- if (settings.network.nodeName.exists(_.isEmpty)) {
                    println("Please, enter node name:")
                    readAndValidateInput(validateNodeName)
                  } else settings.network.nodeName.get.asRight

      mnemonic <- if (settings.wallet.flatMap(_.seed).exists(_.isEmpty)) {
                   val phrase: String = Mnemonic.entropyToMnemonicCode(scorex.utils.Random.randomBytes(16))
                   println(s"Your new mnemonic code is:\n'$phrase' \nPlease, save it and don't show to anyone!")
                   phrase.asRight[Throwable]
                 } else settings.wallet.flatMap(_.seed).get.asRight

    } yield
      InitNodeResult(
        mnemonic,
        walletPassword,
        settings.node.offlineGeneration,
        settings.snapshotSettings.enableFastSynchronization,
        settings.snapshotSettings.enableSnapshotCreation,
        settings.network.knownPeers,
        settings.network.connectOnlyWithKnownPeers.getOrElse(false),
        nodePass,
        frontName,
        settings.network.declaredAddress,
        settings.network.bindAddress
      )
  }

  override def preStart(): Unit = startNode()

  override def receive: Receive = {
    case InitNodeResult(mnemonic,
                        password,
                        offlineGeneration,
                        fastSync,
                        snapshotCreation,
                        peers,
                        connectWithOnlyKnownPeers,
                        nodePass,
                        nodeName,
                        declaredAddr,
                        bindAddr) =>
      import scala.concurrent.duration._
      Functor[Option].compose[Future].map(initHttpApiServer)(_.terminate(3.seconds))
      if (mnemonic.nonEmpty) AccountManager.init(mnemonic, password, settings)
      val walletSettings: Option[WalletSettings] = settings.wallet.map(_.copy(password = password))
      val nodeSettings: NodeSettings             = settings.node.copy(offlineGeneration = offlineGeneration)
      val networkSettings: NetworkSettings =
        settings.network.copy(knownPeers = peers,
                              connectOnlyWithKnownPeers = connectWithOnlyKnownPeers.some,
                              nodeName = nodeName.some,
                              declaredAddress = declaredAddr,
                              bindAddress = bindAddr)

      val snapshotSettings: SnapshotSettings = settings.snapshotSettings.copy(
        enableFastSynchronization = fastSync,
        enableSnapshotCreation = snapshotCreation
      )
      val newSettings = settings.copy(
        wallet = walletSettings,
        node = nodeSettings,
        network = networkSettings,
        snapshotSettings = snapshotSettings
      )
      val influxRef: Option[ActorRef] = newSettings.influxDB.map { influxSettings =>
        context.system
          .actorOf(StatsSender.props(influxSettings, newSettings.network, newSettings.constants), "statsSender")
      }
      lazy val dataHolderForApi =
        context.system.actorOf(DataHolderForApi.props(newSettings, timeProvider), "dataHolder")
      lazy val miner: ActorRef =
        context.system.actorOf(Miner.props(dataHolderForApi, influxRef, newSettings), "miner")
      lazy val memoryPool: ActorRef = context.system.actorOf(
        MemoryPool
          .props(newSettings, timeProvider, miner, influxRef)
          .withDispatcher("mempool-dispatcher")
      )
//      val nodeViewHolder: ActorRef = context.system.actorOf(
//        NodeViewHolder
//          .props(memoryPool, influxRef, dataHolderForApi, newSettings)
//          .withDispatcher("nvh-dispatcher"),
//        "nodeViewHolder"
//      )

      if (nodePass.nonEmpty) dataHolderForApi ! PassForStorage(nodePass)

//      context.system.actorOf(
//        NodeViewSynchronizer
//          .props(influxRef, nodeViewHolder, newSettings, memoryPool, dataHolderForApi)
//          .withDispatcher("nvsh-dispatcher"),
//        "nodeViewSynchronizer"
//      )

      val networkRouter = context.system.actorOf(
        NetworkRouter
          .props(networkSettings, settings.blackList)
          .withDispatcher("nvsh-dispatcher"),
        "networkRouter"
      )

      val nvhRouter = context.system.actorOf(
        IntermediaryNVH.props(newSettings, networkRouter, timeProvider, influxRef)
      )

      if (newSettings.node.mining) miner ! StartMining
      if (newSettings.node.useCli) {
        context.system
          .actorOf(ConsoleListener.props(newSettings, dataHolderForApi, nodeId, timeProvider), "cliListener")
        context.system.actorSelection("/user/cliListener") ! StartListening
      }

      EncryApp.startHttp(dataHolderForApi, memoryPool)
  }
}

object Starter {
  final case class InitNodeResult(mnemonic: String,
                                  walletPassword: String,
                                  offlineGeneration: Boolean,
                                  fastSync: Boolean,
                                  snapshotCreation: Boolean,
                                  peers: List[InetSocketAddress],
                                  connectWithOnlyKnownPeers: Boolean,
                                  nodePass: String = "",
                                  nodeName: String,
                                  declaredAddr: Option[InetSocketAddress],
                                  bindAddr: InetSocketAddress)
}
