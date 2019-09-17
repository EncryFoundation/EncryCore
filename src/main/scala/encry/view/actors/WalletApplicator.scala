package encry.view.actors

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.actors.WalletApplicator.{WalletNeedRollbackTo, WalletNeedScanPersistent}
import encry.view.wallet.EncryWallet
import org.encryfoundation.common.modifiers.PersistentModifier

class WalletApplicator(wallet: EncryWallet, historyApplicator: ActorRef) extends Actor with StrictLogging {

  override def receive: Receive = {
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

  def props(wallet: EncryWallet, historyApplicator: ActorRef): Props =
    Props(new WalletApplicator(wallet, historyApplicator))
}