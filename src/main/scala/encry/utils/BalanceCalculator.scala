package encry.utils

import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import encry.settings.Constants
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey

object BalanceCalculator {

  def balanceSheet(bxs: Traversable[EncryBaseBox]): Map[ADKey, Amount] =
    bxs.foldLeft(Map.empty[ADKey, Amount]) {
      case (cache, bx: AssetBox) =>
        val tokenId = bx.tokenIdOpt.getOrElse(Constants.IntrinsicTokenId)
        cache.get(tokenId).map { amount =>
          cache.updated(tokenId, amount + bx.amount)
        }.getOrElse(cache.updated(tokenId, bx.amount))
      case (cache, _) => cache
    }
}
