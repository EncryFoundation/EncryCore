package encry.utils

import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.modifiers.state.box.{AssetBox, EncryBaseBox, TokenIssuingBox}
import org.encryfoundation.common.utils.Algos

object BalanceCalculator {

  def balanceSheet(bxs: Traversable[EncryBaseBox],
                   defaultTokenId: TokenId,
                   excludeTokenIssuance: Boolean = false): Map[(String, TokenId), Amount] =
    bxs.foldLeft(Map.empty[(String, TokenId), Amount]) {
      case (cache, bx: AssetBox) =>
        val tokenId: TokenId = bx.tokenIdOpt.getOrElse(defaultTokenId)
        val contractHash = Algos.encode(bx.proposition.contractHash)
        cache.get(contractHash -> tokenId).map { amount =>
          cache.updated(contractHash -> tokenId, amount + bx.amount)
        }.getOrElse(cache.updated(contractHash -> tokenId, bx.amount))
      case (cache, bx: TokenIssuingBox) if !excludeTokenIssuance =>
        val contractHash = Algos.encode(bx.proposition.contractHash)
        val tokenId: TokenId = bx.tokenId
        cache.get(contractHash -> tokenId).map { amount =>
          cache.updated(contractHash -> tokenId, amount + bx.amount)
        }.getOrElse(cache.updated(contractHash -> tokenId, bx.amount))
      case (cache, _) => cache
    }.map { case ((hash, id), am) => (hash -> id) -> am }
}
