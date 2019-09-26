package encry.view.actors

import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.actors.StateApplicator.NewWalletForWalletApplicator
import encry.view.actors.WalletApplicator.{GetWallet, WalletNeedRollbackTo, WalletNeedScanPersistent}
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.modifiers.PersistentModifier

class WalletApplicator extends Actor with StrictLogging {

  override def receive: Receive = awaitingWallet

  def awaitingWallet: Receive = {
    case NewWalletForWalletApplicator(wallet) =>
      logger.info("Wallet got wallet storage.")
      context.become(mainBehaviour(wallet))
  }

  def mainBehaviour(wallet: EncryWallet): Receive = {
    case GetWallet => sender() ! wallet
    case WalletNeedRollbackTo(version) =>
      logger.info(s"Wallet need to rollback.")
      wallet.rollback(version)
    case WalletNeedScanPersistent(modifiers) =>
      logger.info(s"Wallet got WalletNeedScanPersistent msg.")
      modifiers.foreach(wallet.scanPersistent)
    case nonsense =>
      logger.info(s"WalletApplicator got from $sender message $nonsense.")
  }
}

object WalletApplicator {

  final case class WalletNeedRollbackTo(version: VersionTag) extends AnyVal
  final case class WalletNeedScanPersistent(modifiers: Seq[PersistentModifier]) extends AnyVal
  case object GetWallet
  def props: Props = Props(new WalletApplicator)
}