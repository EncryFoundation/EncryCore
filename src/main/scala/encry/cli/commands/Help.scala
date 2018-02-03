package encry.cli.commands

import akka.actor.ActorRef
import encry.cli.Response
import encry.settings.EncryAppSettings

object Help extends Command {

  override def execute(nodeViewHolderRef: ActorRef,
                       args: Array[String], settings: EncryAppSettings): Option[Response] =
    Some(Response(
      """
        |Usage: [GROUP_NAME] [COMMAND]=[ARGUMENT]
        |
        |Group name    Command         Argument   Meaning
        |----------------------------------------------------------------
        |node          -stop           None       Stop node
        |wallet        -printPubKeys   None       Prints available public keys
        |wallet        -printPrivKeys  None       Prints available public keys
        |wallet        -init           Seed       Init storage with seed
        |wallet        -addKey         None       Add key to storage
        |wallet        -balance        None       Show balance of current wallet
        |wallet        -transfer       addr;am    Transfer `am`ount to `addr`ess
        |app           -help           None       Show all supported commands
      """
        .stripMargin))
}
