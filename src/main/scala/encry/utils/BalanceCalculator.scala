package encry.utils

import encry.modifiers.state.box.{AssetBox, CoinbaseBox, EncryBaseBox}
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey

object BalanceCalculator {

  private val intrinsicId = ADKey @@ Array.fill(4)(-1: Byte)

  def balanceSheet(bxs: Traversable[EncryBaseBox],
                   excludeCoinbase: Boolean = true): Map[ADKey, Amount] =
    bxs.foldLeft(Map.empty[ADKey, Amount]) {
      case (cache, bx: CoinbaseBox) if !excludeCoinbase =>
        cache.get(intrinsicId).map { amount =>
          cache.updated(intrinsicId, amount + bx.amount)
        }.getOrElse(cache.updated(intrinsicId, bx.amount))
      case (cache, bx: AssetBox) if bx.isIntrinsic =>
        cache.get(intrinsicId).map { amount =>
          cache.updated(intrinsicId, amount + bx.amount)
        }.getOrElse(cache.updated(intrinsicId, bx.amount))
      case (cache, bx: AssetBox) =>
        val tokenId = bx.tokenIdOpt.get
        cache.get(tokenId).map { amount =>
          cache.updated(tokenId, amount + bx.amount)
        }.getOrElse(cache.updated(tokenId, bx.amount))
      case (cache, _) => cache
    }
}
