package encry.utils

import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import encry.settings.Algos

object BalanceCalculator {

  /**
    * Calculate all amount in newBxs
    * @param newBxs
    * @return Map[String, Long] - where String is Algos.encode(tokenId), Long - amount
    *         Long - amount of encryCoin
    */
  def getBalances(newBxs: Seq[EncryBaseBox]): (Map[String, Long], Long) =
    newBxs.foldLeft((Map[String, Long](), 0L)){
    case ((balances, balance), bx) =>
      bx match {
        case aib: AssetBox =>
          aib.tokenIdOpt match {
            case Some(key) => balances.get(Algos.encode(key)) match {
              case Some(v) =>
                (balances.updated(Algos.encode(key), v + aib.amount), balance)
              case None =>
                (balances.updated(Algos.encode(key), aib.amount), balance)
            }
            case None =>
              (balances, balance + aib.amount)
          }
        case _ => (balances, balance)
      }
  }

  def balanceMapToString(balancesMap: Map[String, Long], encryBalance: Long): String = {

    def tokenInfoStr(tokenId: String, balance: Long): String = s"TokenId: $tokenId. Balance: $balance\n"

    tokenInfoStr("Encry coin", encryBalance) ++
      balancesMap.foldLeft(""){ case (str, tokenInfo) => str.concat(tokenInfoStr(tokenInfo._1, tokenInfo._2))}
  }
}
