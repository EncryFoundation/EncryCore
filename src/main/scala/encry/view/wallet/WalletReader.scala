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

  def getBoxById(id: ADKey): Option[AssetBox] = walletStorage.getBoxById(id).toOption

  def getAvailableBoxes: Seq[AssetBox] = walletStorage.getAllBoxes

  def getAllTransactions: Seq[PaymentTransaction] =
    walletStorage.getTransactionIds.foldLeft(Seq[PaymentTransaction]()) { case (buff, id) =>
      val bx = walletStorage.getTransactionById(id)
      if (bx.isSuccess) buff :+ bx.get else buff
    }

  def balance: Long = walletStore.get(WalletStorage.balanceKey).map(v => Longs.fromByteArray(v.data)).getOrElse(0L)
}
