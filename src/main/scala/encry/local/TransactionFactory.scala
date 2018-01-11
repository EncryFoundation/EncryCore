package encry.local

import encry.crypto.Address
import scorex.core.transaction.box.Box.Amount
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Sha256

import scala.util.Random

object TransactionFactory {

  def genRandomBoxes(qty: Int): IndexedSeq[(Address, Amount)] =
    (0 until qty).foldLeft(IndexedSeq[(Address, Amount)]()) { case (bxs, _) =>
      bxs :+ (Address @@ Base58.encode(Sha256(Random.alphanumeric.take(10).mkString)),10L)
    }

  def genUOonPMTrx(qty: Int): IndexedSeq[ADKey] =
    (0 until qty).foldLeft(IndexedSeq[ADKey]()) { case (keys, _) =>
      keys :+ (ADKey @@ Sha256(Random.alphanumeric.take(10).mkString))
    }
}
