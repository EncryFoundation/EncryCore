package encry.cli

import akka.actor.{Actor, ActorRef, Props}
import encry.cli.commands._
import encry.cli.commands.history.{
  GetCandidate,
  GetFullBlockById,
  GetHeaderById,
  GetLastHeaderIdsAtHeight,
  GetLastHeaders,
  GetTxById}
import encry.cli.commands.info.GetInfo
import encry.cli.commands.peer.{GetBan, GetBannedPeers, GetConnectedPeers, GetPeers}
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class ConsoleListener(settings: EncryAppSettings,
                      dataHolder: ActorRef,
                      nodeId: Array[Byte],
                      networkTimeProvider: NetworkTimeProvider)
    extends Actor {

  import ConsoleListener._

  override def receive: Receive = {
    case StartListening =>
      Iterator.continually(scala.io.StdIn.readLine(prompt)).foreach { input =>
        InputParser.parse(input) match {
          case Success(command) =>
            getCommand(command.category.name, command.ident.name) match {
              case Some(cmd) =>
                cmd
                  .execute(Command.Args(command.params.map(p => p.ident.name -> p.value).toMap),
                           settings,
                           dataHolder,
                           nodeId,
                           networkTimeProvider)
                  .map {
                    case Some(x) => print(x.msg + s"\n$prompt")
                    case None    =>
                  }
              case None => println("Unknown command. Type 'app help' to see command list.")
            }
          case Failure(_) =>
        }
      }
  }
}

object ConsoleListener {

  case object StartListening

  val prompt = "$> "

  def getCommand(cat: String, cmd: String): Option[Command] = cmdDictionary.get(cat).flatMap(_.get(cmd))

  private val nodeCmds = Map(
    "node" -> Map(
      "shutdown"    -> NodeShutdown,
      "stopMining"  -> StopMining,
      "startMining" -> StartMining,
    )
  )

  private val appCmds = Map(
    "app" -> Map(
      "help" -> Help,
      "info" -> GetInfo
    )
  )
// link "wallet"
  private val walletCmds = Map(
    "wallet" -> Map(
      "addrs"       -> PrintAddresses,
      "createKey"   -> CreateKey,
      "pubKeys"     -> PrintPubKeys,
      "balance"     -> GetBalance,
      "transfer"    -> Transfer,
      "privKeys"    -> PrintPrivKeys, //Todo delete
      "createToken" -> CreateToken,
      "fromSeed"  -> CreateAccountFromSeed
    )
  )

  private val historyCmds = Map(
    "history" -> Map(
      "getLastHeaders"   -> GetLastHeaders,
      "getLastHeaderIds" -> GetLastHeaderIdsAtHeight,
      "getHeaderById"    -> GetHeaderById,
      "getTxById"        -> GetTxById,
      "getCandidate"     -> GetCandidate,
      "getFullBlock"     -> GetFullBlockById
    )
  )

  private val peerCmds = Map(
    "peer" -> Map(
      "all"                 -> GetPeers,
      "banned"              -> GetBannedPeers,
      "connected"           -> GetConnectedPeers,
      "ban"                 -> GetBan,
      "addPeer"             -> AddPeer,
      "removeFromBlackList" -> RemoveFromBlackList
    )
  )

  val cmdDictionary: Map[String, Map[String, Command]] =
    ConsoleListener.nodeCmds ++ ConsoleListener.appCmds ++ ConsoleListener.walletCmds ++ ConsoleListener.historyCmds ++
      ConsoleListener.peerCmds

  def props(settings: EncryAppSettings,
            dataHolder: ActorRef,
            nodeId: Array[Byte],
            timeProvider: NetworkTimeProvider): Props =
    Props(new ConsoleListener(settings, dataHolder, nodeId, timeProvider))
}