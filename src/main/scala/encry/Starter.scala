package encry

import akka.actor.{ Actor, ActorRef }
import encry.settings.{ EncryAppSettings, NetworkSettings, NodeSettings, SnapshotSettings, WalletSettings }
import java.io.File
import java.net.InetSocketAddress
import java.nio.file.Files

import akka.http.scaladsl.Http
import encry.api.http.DataHolderForApi
import encry.cli.ConsoleListener
import encry.cli.ConsoleListener.{ prompt, StartListening }
import encry.local.miner.Miner
import encry.local.miner.Miner.StartMining
import encry.network.NodeViewSynchronizer
import encry.utils.{ Mnemonic, NetworkTimeProvider }
import encry.view.NodeViewHolder
import encry.view.mempool.MemoryPool

import cats.Functor
import cats.syntax.option._
import cats.syntax.either._
import cats.data.{ NonEmptyChain, Validated }
import cats.syntax.validated._
import cats.instances.string._
import cats.syntax.apply._
import cats.instances.option._
import cats.instances.future._

import scala.concurrent.Future
import encry.Starter.InitNodeResult
import encry.view.wallet.AccountManager

import scala.io.StdIn

class Starter(settings: EncryAppSettings,
              timeProvider: NetworkTimeProvider,
              influxRef: Option[ActorRef],
              nodeId: Array[Byte])
    extends Actor {

  import context.dispatcher

  var initHttpApiServer: Option[Future[Http.ServerBinding]] = none

  def startNode(): Unit =
    (for {
      result <- if (!Files.exists(new File(s"${settings.directory}/state").toPath))
                 startEmptyNode
               else
                 startNonEmptyNode
    } yield result) match {
      case Left(ex) =>
        println(s"Node start with http api. $ex")
      case Right(res) =>
        println("Node config read successfully!")
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
        settings.wallet.flatMap(_.seed).getOrElse(""),
        walletPassword,
        settings.node.offlineGeneration,
        fastSync = false,
        settings.network.knownPeers.toList
      )

  def startEmptyNode: Either[Throwable, InitNodeResult] =
    for {
      answer <- {
        println("Would you like to start your node with http api help or with cli? Enter yes or no:")
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
          println("Your answer is not matched with yes/no answer. Please, enter it again")
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
        println(s"Your input is incorrect cause: ${validatedData.show}")
        readAndValidateInput(validationFunction)
      }
    }
    Either
      .catchNonFatal(StdIn.readLine(prompt).toLowerCase)
      .fold(handleError, handleResult)
  }

  def validatePassword(password: String): Validated[NonEmptyChain[String], String] =
    if (password.nonEmpty) password.validNec else "Password is empty".invalidNec

  def startWithCli: Either[Throwable, InitNodeResult] = {

    def validateMnemonicKey(mnemonic: String): Validated[NonEmptyChain[String], String] = {
      val words: Array[String] = mnemonic.split(" ")
      val isValidSize: Validated[NonEmptyChain[String], String] =
        if (words.length == 12) mnemonic.validNec else "Wrong words size".invalidNec
      val isValidWords: Validated[NonEmptyChain[String], String] =
        if (words.forall(word => Mnemonic.getWords.contains(word))) mnemonic.validNec
        else "Several words don't contain in available words".invalidNec
      (isValidSize, isValidWords).mapN { case (_, mnemonic) => mnemonic }
    }

    def readPeersToConnect: Either[Throwable, List[InetSocketAddress]] = {
      def loop(peers: List[InetSocketAddress]): Either[Throwable, List[InetSocketAddress]] = {
        def handleError: Throwable => Either[Throwable, List[InetSocketAddress]] = (ex: Throwable) => {
          println(s"You entered incorrect input cause: $ex. Enter it again")
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
          println("Enter port:")
          val port = StdIn.readLine(prompt).toInt
          println("Enter host:")
          val host = StdIn.readLine(prompt)
          new InetSocketAddress(host, port)
        }.fold(handleError, handleResult)
      }

      loop(List.empty[InetSocketAddress])
    }

    for {
      walletPassword <- { println("Please, enter wallet password:"); readAndValidateInput(validatePassword) }
      mnemonicKeyAnswer <- {
        println("Would you like to enter your mnemonic key or generate new one? yes - if your own otherwise no")
        readAnswer
      }
      mnemonicKey <- if (mnemonicKeyAnswer) {
                      println("Enter your mnemonic key:")
                      readAndValidateInput(validateMnemonicKey)
                    } else {
                      val phrase: String = Mnemonic.entropyToMnemonicCode(scorex.utils.Random.randomBytes(16))
                      println(s"\nYour new mnemonic code is:\n$phrase. Please, save it and don't show to anyone!")
                      phrase.asRight[Throwable]
                    }
      startOwnChain <- {
        println("Would you like to start your own chain? yes - if start your won, otherwise no")
        readAnswer
      }
      enableFastSync <- if (startOwnChain) false.asRight[Throwable]
                       else {
                         println("Would you like to enable fast sync? yes or no")
                         readAnswer
                       }
      answerPeers <- {
        println("Would you like to enter peers to connect with? yes or no")
        readAnswer
      }
      peers <- if (answerPeers) readPeersToConnect else List.empty[InetSocketAddress].asRight[Throwable]
    } yield InitNodeResult(mnemonicKey, walletPassword, startOwnChain, enableFastSync, peers)
  }

  def startWithHttpApi: Either[Throwable, InitNodeResult] = {
    println("Empty node will start with HTTP Api.")
    initHttpApiServer = EncryApp.tmpServer(self).some
    println(s"Server started at: 0.0.0.0:9051/something")
    new Exception("Node started with http api").asLeft[InitNodeResult]
  }

  override def preStart(): Unit = startNode()

  override def receive: Receive = {
    case InitNodeResult(mnemonic, password, offlineGeneration, fastSync, peers) =>
      println("Got accumulated info.")
      Functor[Option].compose[Future].map(initHttpApiServer)(_.unbind())
      AccountManager.init(mnemonic, password, settings)
      //todo enableFastSynchronization bug?
      println(fastSync)
      val walletSettings: Option[WalletSettings] = settings.wallet.map(_.copy(password = password))
      val nodeSettings: NodeSettings             = settings.node.copy(offlineGeneration = offlineGeneration)
      val networkSettings: NetworkSettings       = settings.network.copy(knownPeers = peers)
      val snapshotSettings: SnapshotSettings     = settings.snapshotSettings.copy(enableFastSynchronization = fastSync)
      val newSettings = settings.copy(
        wallet = walletSettings,
        node = nodeSettings,
        network = networkSettings,
        snapshotSettings = snapshotSettings
      )
      lazy val dataHolderForApi =
        context.system.actorOf(DataHolderForApi.props(newSettings, timeProvider), "dataHolder")
      lazy val miner: ActorRef =
        context.system.actorOf(Miner.props(dataHolderForApi, influxRef, newSettings), "miner")
      lazy val memoryPool: ActorRef = context.system.actorOf(
        MemoryPool
          .props(newSettings, timeProvider, miner, influxRef)
          .withDispatcher("mempool-dispatcher")
      )
      val nodeViewHolder: ActorRef = context.system.actorOf(
        NodeViewHolder
          .props(memoryPool, influxRef, dataHolderForApi, newSettings)
          .withDispatcher("nvh-dispatcher"),
        "nodeViewHolder"
      )

      val nodeViewSynchronizer: ActorRef = context.system.actorOf(
        NodeViewSynchronizer
          .props(influxRef, nodeViewHolder, newSettings, memoryPool, dataHolderForApi)
          .withDispatcher("nvsh-dispatcher"),
        "nodeViewSynchronizer"
      )

      if (settings.node.mining) miner ! StartMining
      if (settings.node.useCli) {
        context.system.actorOf(ConsoleListener.props(settings, dataHolderForApi, nodeId, timeProvider), "cliListener")
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
                                  peers: List[InetSocketAddress])
}
