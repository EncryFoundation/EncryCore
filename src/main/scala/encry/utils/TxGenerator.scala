package encry.utils

import encry.modifiers.mempool.{Transaction, TransactionFactory}
import encry.modifiers.state.box.AssetBox
import encry.modifiers.state.box.Box.Amount
import org.encryfoundation.common.crypto.PrivateKey25519

object TxGenerator {

  val txFee: Amount = 100

  def generateDataTxs(data: Seq[Array[Byte]],
                      avaliableBoxes: Seq[AssetBox],
                      privKey: PrivateKey25519): Seq[Transaction] = {
    val dataWithFeeBoxes: Seq[(Array[Byte], Seq[AssetBox])] =
      data.foldLeft(Seq[(Array[Byte], Seq[AssetBox])](), avaliableBoxes) {
        case ((completeDataWithFees, boxesToSpend), dataBytes) =>
          val boxes = boxesToSpend.foldLeft(Seq[AssetBox]()) {
            case (acceptableBoxes, nextBox) =>
              if (acceptableBoxes.map(_.amount).sum < txFee) acceptableBoxes :+ nextBox
              else acceptableBoxes
          }
          (completeDataWithFees :+ (dataBytes, boxes), boxesToSpend.diff(boxes))
      }._1
    dataWithFeeBoxes.map(dataWithBoxes =>
      TransactionFactory.defaultDataTransactionScratch(privKey,
        txFee,
        System.currentTimeMillis(),
        dataWithBoxes._1,
        dataWithBoxes._2.toIndexedSeq)
    )
  }

}
