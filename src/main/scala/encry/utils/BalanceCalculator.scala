package encry.utils

import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import encry.settings.Constants

object BalanceCalculator {

  def balanceSheet(bxs: Traversable[EncryBaseBox]): Map[TokenId, Amount] =
    bxs.foldLeft(Map.empty[ByteStr, Amount]) {
      case (cache, bx: AssetBox) =>
        val tokenId: ByteStr = ByteStr(bx.tokenIdOpt.getOrElse(Constants.IntrinsicTokenId))
        cache.get(tokenId).map { amount =>
          cache.updated(tokenId, amount + bx.amount)
        }.getOrElse(cache.updated(tokenId, bx.amount))
      case (cache, _) => cache
    }.map { case (id, am) => id.arr -> am }
}
