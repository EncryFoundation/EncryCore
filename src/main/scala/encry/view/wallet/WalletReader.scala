package encry.view.wallet

import com.google.common.primitives.Longs
import encry.modifiers.mempool.PaymentTransaction
import encry.modifiers.state.box.AssetBox
import encry.view.wallet.storage.WalletStorage
import io.iohk.iodb.Store
import scorex.crypto.authds.ADKey

trait WalletReader {

  val walletStore: Store

  val walletStorage: WalletStorage

  def getBoxById(id: ADKey): Option[AssetBox] = walletStorage.getBoxById(id)

  def getAvailableBoxes: Seq[AssetBox] = walletStorage.getAllBoxes

  def getAllTransactions: Seq[PaymentTransaction] =
    walletStorage.getTransactionIds.foldLeft(Seq[PaymentTransaction]()) { case (buff, id) =>
      val bxOpt = walletStorage.getTransactionById(id)
      if (bxOpt.isDefined) buff :+ bxOpt.get else buff
    }

  def balance: Long = walletStore.get(WalletStorage.balanceKey).map(v => Longs.fromByteArray(v.data)).getOrElse(0L)
}
