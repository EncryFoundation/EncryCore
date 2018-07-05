package encry.cli

import java.security.SecureRandom

import encry.EncryApp
import encry.EncryApp.{miner, timeProvider, _}
import encry.account.Address
import encry.local.miner.EncryMiner.EnableMining
import encry.settings.EncryAppSettings
import encry.utils.Mnemonic
import encry.utils.NetworkTime.Time

import scala.concurrent.Future

object Commands {

  object LocalCommand

  object AddKey extends ViewCommand

  object GetBalance extends ViewCommand

  object PrintMyAddrs extends ViewCommand

  //TODO This cmd is unsafe.
  object PrintPrivKeys extends ViewCommand

  case object PrintPubKeys extends ViewCommand

  object NodeShutdown extends Command {

    override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
      EncryApp.forceStopApplication()
      Future(None)
    }
  }

  object StartMine extends Command {

    import encry.local.miner.EncryMiner.StartMining

    override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
      miner ! EnableMining
      miner ! StartMining
      Future(Some(Response("Mining is started.")))
    }
  }

  object StopMine extends Command {

    import encry.local.miner.EncryMiner.DisableMining

    override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] = {
      miner ! DisableMining
      Future(Some(Response("Mining is stopped.")))
    }
  }

  object Transfer extends ViewCommand {

    /**
      * Command "wallet transfer -addr=<addr[String]> -fee=<fee[Num]> -amount=<amount[Num]>"
      * Example "wallet transfer -addr='3jSD9fwHEHJwHq99ARqhnNhqGXeKnkJMyX4FZjHV6L3PjbCmjG' -fee=10000 -amount=2000"
      */
    // TODO: Notify `Vault` of spent boxes.
    override def executeRequest(args: Command.Args, settings: EncryAppSettings): Request = {
      Request(
        Address @@ args.requireArg[Ast.Str]("addr").s,
        args.requireArg[Ast.Num]("fee").i,
        args.requireArg[Ast.Num]("amount").i,
        timeProvider.time()
      )
    }

    case class Request(recipient: Address, fee: Long, amount: Long, timestamp: Time)
  }

  object InitKeyStorage extends ViewCommand {

    case class Request(mnemonicCode: String)

    override def executeRequest(args: Command.Args, settings: EncryAppSettings): Any =
      Request(args.requireArgOrElse("seed", Ast.Str(Mnemonic.entropyToMnemonicCode(SecureRandom.getSeed(16)))).s)

  }

  object Help extends Command {

    override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] =
      Future(Some(Response(
        """
          |Usage: [GROUP_NAME] [COMMAND] -[ARGUMENT_1]=[VAL_1] -[ARGUMENT_2]=[VAL_2]
          |
          |Group name    Command          Argument       Meaning
          |--------------------------------------------------------------------------------
          |node          shutdown         None           Shutdown the node
          |node          stopMining       None           Node stops mining
          |node          startMining      None           Node starts mining
          |wallet        pubKeys          None           Print available public keys
          |wallet        addrs            None           Print available addresses
          |wallet        init             seed           Init storage with seed
          |wallet        init             None           Generate new storage
          |wallet        addKey           None           Add key to storage
          |wallet        balance          None           Show balance of current wallet
          |wallet        transfer         addr, amount   Transfer `amount` to `addr`ess
          |app           help             None           Show all supported commands
        """
          .stripMargin)))
  }
}
