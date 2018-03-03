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

  def availableBoxes: Seq[EncryBaseBox] = walletStorage.allBoxes

  def allTransactions: Seq[EncryBaseTransaction] =
    walletStorage.transactionIds.foldLeft(Seq[EncryBaseTransaction]()) { case (acc, id) =>
      walletStorage.getTransactionById(id).map(bx => acc :+ bx).getOrElse(acc)
    }

  def balance: Long = walletStore.get(WalletStorage.balanceKey).map(v => Longs.fromByteArray(v.data)).getOrElse(0L)
}
