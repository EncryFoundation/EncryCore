package encry.utils

import encry.modifiers.state.box.{AssetBox, CoinbaseBox, EncryBaseBox}
import encry.settings.Algos
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32

object BalanceCalculator {

  val intrinsicTokenId: ADKey = ADKey @@ Algos.hash("intrinsic_token")

  def balanceSheet(bxs: Traversable[EncryBaseBox],
                   excludeCoinbase: Boolean = true): Map[ADKey, Amount] =
    bxs.foldLeft(Map.empty[ADKey, Amount]) {
      case (cache, bx: CoinbaseBox) if !excludeCoinbase =>
        cache.get(intrinsicTokenId).map { amount =>
          cache.updated(intrinsicTokenId, amount + bx.amount)
        }.getOrElse(cache.updated(intrinsicTokenId, bx.amount))
      case (cache, bx: AssetBox) =>
        val tokenId = bx.tokenIdOpt.getOrElse(intrinsicTokenId)
        cache.get(tokenId).map { amount =>
          cache.updated(tokenId, amount + bx.amount)
        }.getOrElse(cache.updated(tokenId, bx.amount))
      case (cache, _) => cache
    }
}
