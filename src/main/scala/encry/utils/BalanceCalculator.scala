package encry.utils

import encry.settings.Constants
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryBaseBox, TokenIssuingBox}

object BalanceCalculator {

  def balanceSheet(bxs: Traversable[EncryBaseBox],
                   excludeTokenIssuance: Boolean = false): Map[TokenId, Amount] =
    bxs.foldLeft(Map.empty[ByteStr, Amount]) {
      case (cache, bx: AssetBox) =>
        val tokenId: ByteStr = ByteStr(bx.tokenIdOpt.getOrElse(Constants.IntrinsicTokenId))
        cache.get(tokenId).map { amount =>
          cache.updated(tokenId, amount + bx.amount)
        }.getOrElse(cache.updated(tokenId, bx.amount))
      case (cache, bx: TokenIssuingBox) if !excludeTokenIssuance =>
        val tokenId: ByteStr = ByteStr(bx.tokenId)
        cache.get(tokenId).map { amount =>
          cache.updated(tokenId, amount + bx.amount)
        }.getOrElse(cache.updated(tokenId, bx.amount))
      case (cache, _) => cache
    }.map { case (id, am) => id.arr -> am }
}
