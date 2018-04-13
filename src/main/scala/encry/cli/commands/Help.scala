package encry.cli.commands

import akka.actor.ActorRef
import encry.cli.Response
import encry.settings.EncryAppSettings

object Help extends Command {

  override def execute(nodeViewHolderRef: ActorRef,
                       args: Array[String], settings: EncryAppSettings): Option[Response] =
    Some(Response(
      """
        |Usage: [GROUP_NAME] [COMMAND]=[ARGUMENT_1];[ARGUMENT_2]
        |
        |Group name    Command          Argument       Meaning
        |--------------------------------------------------------------------------------
        |node          -shutdown        None           Shutdown the node
        |wallet        -pubKeys         None           Print available public keys
        |wallet        -addrs           None           Print available addresses
        |wallet        -init            w1;w2;..       Init storage with seed
        |wallet        -init            None           Generate new storage
        |wallet        -addKey          None           Add key to storage
        |wallet        -balance         None           Show balance of current wallet
        |wallet        -transfer        addr;amount    Transfer `amount` to `addr`ess
        |wallet        -addPubKeyInfo   pk;ppk;pki;f   Add pub key info to the blockchain
        |app           -help            None           Show all supported commands
      """
        .stripMargin))
}
