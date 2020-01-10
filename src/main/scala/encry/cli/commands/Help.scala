package encry.cli.commands

import akka.actor.ActorRef
import encry.cli.Response
import encry.settings.EncryAppSettings
import encry.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Help extends Command {

  /**
    * Command "app help"
    */
  override def execute(args: Command.Args, settings: EncryAppSettings, dataHolder: ActorRef,nodeId: Array[Byte],
                       networkTimeProvider: NetworkTimeProvider): Future[Option[Response]] =
    Future(Some(Response(
      """
        |Usage: [GROUP_NAME] [COMMAND] -[ARGUMENT_1]=[VAL_1] -[ARGUMENT_2]=[VAL_2]
        |
        |Group name    Command              Argument       Meaning
        |----------------------------------------------------------------------------------
        |node          shutdown             None           Shutdown the node
        |node          stopMining           None           Node stops mining
        |node          startMining          None           Node starts mining
        |wallet        pubKeys              None           Print available public keys
        |wallet        privKeys             None           Print available private keys
        |wallet        addrs                None           Print available addresses
        |wallet        createKey            None           Add key to storage
        |wallet        balance              None           Show balance of current wallet
        |wallet        transfer             addr, amount   Transfer `amount` to `address`
        |wallet        createToken          fee, amount    Creates new token
        |wallet        createKey            None           Creates new account
        |wallet        fromSeed             Seed           Creates new account from seed
        |peer          removeFromBlackList  host, port     Remove peer from black list
        |peer          addPeer              host, port     Add peer to 'knownPeers'
        |peer          all                  None           Get all peers in the system
        |peer          connected            None           Get all connected peers
        |peer          banned               None           Get all banned peers
        |peer          ban                  host, port     Ban peer
        |app           info                 None           Show info about your node
        |app           help                 None           Show all supported commands
        |history       getTxById            Id             Get transaction by 'Id'
        |history       getLastHeaders       Number         Get last 'Number' headers
        |history       getLastHeaderIds     Number         Get header at height 'Number'
        |history       getHeaderById        Id             Get header by 'Id'
        |history       getFullBlock         Id             Get block by 'Id'
        |history       getTxById            None           Get block candidate
      """
        .stripMargin)))
}
