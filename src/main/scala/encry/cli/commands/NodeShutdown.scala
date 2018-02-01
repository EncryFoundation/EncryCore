package encry.cli.commands
import akka.actor.ActorRef
import akka.util.Timeout
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import encry.view.mempool.EncryMempool
import encry.view.state.UtxoState
import encry.view.wallet.EncryWallet
import scorex.core.NodeViewHolder

import scala.util.Try

object NodeShutdown extends Command {

  override def execute(nodeViewHolderRef: ActorRef, args: Array[String], settings: EncryAppSettings): Try[Unit] = Try(System.exit(0))

}
