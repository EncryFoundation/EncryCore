package encry.view.wallet

import com.google.common.primitives.Longs
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.{CoinbaseBox, EncryBaseBox}
import encry.view.history.Height
import encry.view.wallet.storage.WalletStorage
import io.iohk.iodb.Store
import scorex.crypto.authds.ADKey

trait WalletReader {

  val walletStore: Store

  val walletStorage: WalletStorage

  def getBoxById(id: ADKey): Option[EncryBaseBox] = walletStorage.getBoxById(id)

  def availableBoxes: Seq[EncryBaseBox] = walletStorage.allBoxes

  def getAvailableCoinbaseBoxesAt(h: Height): Seq[CoinbaseBox] =
    walletStorage.openBoxIds.foldLeft(Seq[CoinbaseBox]()) { case (acc, id) =>
        walletStorage.getBoxById(id).map {
          case bx: CoinbaseBox if bx.proposition.height <= h => acc :+ bx
          case _ => acc
        }.getOrElse(acc)
      }

  def allTransactions: Seq[EncryBaseTransaction] =
    walletStorage.transactionIds.foldLeft(Seq[EncryBaseTransaction]()) { case (acc, id) =>
      walletStorage.getTransactionById(id).map(tx => acc :+ tx).getOrElse(acc)
    }

  def balance: Long = walletStore.get(WalletStorage.balanceKey).map(v => Longs.fromByteArray(v.data)).getOrElse(0L)
}
