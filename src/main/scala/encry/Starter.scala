package encry

import java.nio.file.Files

import scala.io._
import akka.actor.{Actor, ActorRef}
import encry.settings.{EncryAppSettings, WalletSettings}
import java.io.File
import java.net.InetSocketAddress

import akka.http.scaladsl.Http
import encry.EncryApp.{influxRef, nodeId, system, timeProvider}
import encry.api.http.DataHolderForApi
import encry.cli.ConsoleListener
import encry.cli.ConsoleListener.{StartListening, prompt}
import encry.local.miner.Miner
import encry.local.miner.Miner.StartMining
import encry.network.NodeViewSynchronizer
import encry.utils.Mnemonic
import encry.view.NodeViewHolder
import encry.view.mempool.MemoryPool

import scala.concurrent.Future

class Starter(settings: EncryAppSettings) extends Actor {

  var xuiTmp: Option[Future[Http.ServerBinding]] = None

  override def preStart(): Unit = {
    val stateDir: File = new File(s"${settings.directory}/state")
    if (!Files.exists(stateDir.toPath)) {
      //empty node
      println("Would you like to start node with FRONT APLI? Yes/No")
      val startAnswer = StdIn.readLine(prompt)
      if (startAnswer == "Yes") {
        //start with http api


        println("Start api for init")
        val tmpServer: Future[Http.ServerBinding] = EncryApp.tmpServer(self)
        xuiTmp = Some(tmpServer)


        println(s"StartNode with front")
        //simple await msg from http
      } else {
        //start with cli
        println("Would you like set up password? Yes/No")
        val passAnswer = StdIn.readLine(prompt)
        val pass = if (passAnswer == "Yes") {
          println("Enter your pass")
          StdIn.readLine(prompt)
        } else ""
        println("Would you like enter mnemonic key? Yes/No")
        val mnemonicAnswer = StdIn.readLine(prompt)
        val mnemonic = if (mnemonicAnswer == "Yes") {
          println("Enter your mnemonic")
          StdIn.readLine(prompt)
        } else {
          val phrase: String = Mnemonic.entropyToMnemonicCode(scorex.utils.Random.randomBytes(16))
          println(s"\nMnemonic code is:\n$phrase")
          phrase
        }
        println("Would you like to start new chain? Yes/No")
        val offChainAnswer = StdIn.readLine(prompt) match {
          case "Yes" => true
          case _     => false
        }
        println("Enter type of sync. Fast/Normal")
        val syncTypeAnswer = StdIn.readLine(prompt) match {
          case "Fast" => true
          case _      => false
        }
        println(s"Enter peer to connect: First port(9999), next host(172.99.99.99)")
        val port     = StdIn.readLine("port: ")
        val host     = StdIn.readLine("host: ")
        val peer     = new InetSocketAddress(host, port.toInt)
        self ! InitRes(mnemonic, pass, offChainAnswer, Seq(peer), syncTypeAnswer)
      }
    } else {
      //full node
      println("Start non empty node")
      println("Enter wallet password. If doesn't exist - empty string")
      val walletPass = StdIn.readLine(prompt)
      self ! InitRes(
        settings.wallet.flatMap(_.seed).getOrElse(""),
        walletPass,
        settings.node.offlineGeneration,
        settings.network.knownPeers,
        fastSync = false
      )
    }
  }

  case class InitRes(mnemonic: String, pass: String, offGen: Boolean, peers: Seq[InetSocketAddress], fastSync: Boolean)

  override def receive: Receive = {
    case InitRes(mnemonic, pass, offChainAnswer, peers, syncTypeAnswer) =>
      import context.dispatcher
      xuiTmp.map(_.map(_.unbind()))
      val walletSettings: Option[WalletSettings] = settings.wallet.map(w =>
        w.copy(password = pass, seed = if (mnemonic.nonEmpty) Some(mnemonic) else None)
      )
      //todo enableFastSynchronization bug
      val nodeSettings = settings.node.copy(offlineGeneration = offChainAnswer)
      val networkSt = settings.network.copy(knownPeers = peers)
      val snapshotSet = settings.snapshotSettings.copy(enableFastSynchronization = syncTypeAnswer)
      val setNew = settings.copy(
        wallet = walletSettings,
        node = nodeSettings,
        network = networkSt,
        snapshotSettings = snapshotSet
      )
      lazy val dataHolderForApi = system.actorOf(DataHolderForApi.props(setNew, timeProvider), "dataHolder")
      lazy val miner: ActorRef = system.actorOf(Miner.props(dataHolderForApi, influxRef, setNew), "miner")
      lazy val memoryPool: ActorRef = system.actorOf(MemoryPool.props(setNew, timeProvider, miner, influxRef)
        .withDispatcher("mempool-dispatcher"))
      val nodeViewHolder: ActorRef = system.actorOf(NodeViewHolder.props(memoryPool, influxRef, dataHolderForApi, setNew)
        .withDispatcher("nvh-dispatcher"), "nodeViewHolder")

      val nodeViewSynchronizer: ActorRef = system.actorOf(NodeViewSynchronizer
        .props(influxRef, nodeViewHolder, setNew, memoryPool, dataHolderForApi)
        .withDispatcher("nvsh-dispatcher"), "nodeViewSynchronizer")

      if (settings.node.mining) miner ! StartMining
      if (settings.node.useCli) {
        system.actorOf(ConsoleListener.props(settings, dataHolderForApi, nodeId, timeProvider), "cliListener")
        system.actorSelection("/user/cliListener") ! StartListening
      }

      EncryApp.startHttp(dataHolderForApi, memoryPool)

    case xui => println("pizda prishla")

  }
}
