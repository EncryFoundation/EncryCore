package encry.it.contracts

import org.scalatest.FunSuite

class Contract extends FunSuite {

  test("") {

    val contract =
      s"""
         |contract(lowerBound: Int, upperBound: Int, transaction: Transaction, state: State, tokenId: String, transactionFee: Int) = {
         |
         |  let encryCoinId = "487291c237b68dd2ab213be6b5d1174666074a5afab772b600ea14e8285affab"
         |
         |  def boxesWithTokens(bx: Box) = {
         |    if (let ab: AssetBox = bx) {
         |      ab.tokenId == tokenId
         |    } else false
         |  }
         |
         |  def boxesWithEncryCoins(bx: Box) = {
         |    if (let ab: AssetBox = bx) {
         |      ab.tokenId == encryCoinId
         |    } else false
         |  }
         |
         |  if (state.height < upperBound && state.height >= lowerBound &&
         |      transaction.inputs.filter(boxesWithTokens).map(lamb(x: AssetBox) = x.amount).sum > 50 &&
         |      transaction.outputs.filter(boxesWithTokens).size == 0) { true }
         |  elif (state.height > upperBound &&
         |        transaction.outputs.filter(boxesWithEncryCoins).map(lamb(x: AssetBox) = x.amount).sum - transactionFee == 1)
         |        { true }
         |  else false
         |}
       """.stripMargin
  }
}