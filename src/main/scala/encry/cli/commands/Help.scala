package encry.cli.commands

import encry.cli.Response
import encry.settings.EncryAppSettings
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Help extends Command {

  override def execute(args: Command.Args, settings: EncryAppSettings): Future[Option[Response]] =
    Future(Some(Response(
      """
        |Usage: [GROUP_NAME] [COMMAND] -[ARGUMENT_1]=[VAL_1] -[ARGUMENT_2]=[VAL_2]
        |
        |Group name    Command          Argument       Meaning
        |--------------------------------------------------------------------------------
        |node          shutdown         None           Shutdown the node
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
