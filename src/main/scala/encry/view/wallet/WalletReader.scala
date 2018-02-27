package encry.view.wallet

import com.google.common.primitives.Longs
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.EncryBaseBox
import encry.view.wallet.storage.WalletStorage
import io.iohk.iodb.Store
import scorex.crypto.authds.ADKey

trait WalletReader {

  val walletStore: Store

  val walletStorage: WalletStorage

  def getBoxById(id: ADKey): Option[EncryBaseBox] = walletStorage.getBoxById(id)

  def getAvailableBoxes: Seq[EncryBaseBox] = walletStorage.getAllBoxes

  def getAllTransactions: Seq[EncryBaseTransaction] =
    walletStorage.getTransactionIds.foldLeft(Seq[EncryBaseTransaction]()) { case (buff, id) =>
      val bxOpt = walletStorage.getTransactionById(id)
      if (bxOpt.isDefined) buff :+ bxOpt.get else buff
    }

  def balance: Long = walletStore.get(WalletStorage.balanceKey).map(v => Longs.fromByteArray(v.data)).getOrElse(0L)
}
