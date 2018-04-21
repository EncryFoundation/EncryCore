package encry.utils

import encry.modifiers.state.box.{AssetBox, CoinbaseBox, EncryBaseBox}
import encry.settings.Algos
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32

object BalanceCalculator {

  val encryCoinKey: ADKey = ADKey @@ Algos.hash("EncryCoin").untag(Digest32)

  def balanceSheet(bxs: Traversable[EncryBaseBox],
                   excludeCoinbase: Boolean = true): Map[ADKey, Amount] =
    bxs.foldLeft(Map.empty[ADKey, Amount]) {
      case (cache, bx: CoinbaseBox) if !excludeCoinbase =>
        cache.get(encryCoinKey).map { amount =>
          cache.updated(encryCoinKey, amount + bx.amount)
        }.getOrElse(cache.updated(encryCoinKey, bx.amount))
      case (cache, bx: AssetBox) =>
        val tokenId = bx.tokenIdOpt.getOrElse(encryCoinKey)
        cache.get(tokenId).map { amount =>
          cache.updated(tokenId, amount + bx.amount)
        }.getOrElse(cache.updated(tokenId, bx.amount))
      case (cache, _) => cache
    }
}
