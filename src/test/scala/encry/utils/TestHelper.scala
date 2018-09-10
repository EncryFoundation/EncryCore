package encry.utils

import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, EncryBaseBox, EncryProposition}
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.transaction.EncryAddress
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scala.util.Random

object TestHelper {

  val genesisSeed: Long = Long.MaxValue

  object Props {
    val keysQty: Int = 1000
    val boxValue: Amount = 1000000
    val txAmount: Amount = 199000
    val txFee: Amount = 4300
  }

  def genKeys(qty: Int): Seq[PrivateKey25519] = {
    val rnd: Random = new scala.util.Random(genesisSeed)
    (0 to qty)
      .foldLeft(Seq[PrivateKey25519]()) { case (acc, _) =>
        val keys: (PrivateKey, PublicKey) = Curve25519.createKeyPair(
          rnd.alphanumeric.take(32).mkString.getBytes)
        acc :+ PrivateKey25519(keys._1, keys._2)
      }
  }

  def genAssetBoxes: IndexedSeq[AssetBox] = {
    val rnd: Random = new scala.util.Random(genesisSeed)
    genKeys(Props.keysQty).foldLeft(IndexedSeq[AssetBox]()) { case (bxs, pk) =>
      bxs :+ AssetBox(
        EncryProposition.pubKeyLocked(pk.publicKeyBytes),
        rnd.nextLong(), Props.boxValue)
    }
  }

  def genAssetBox(address: EncryAddress.Address, amount: Amount = 9L): AssetBox =
    AssetBox(EncryProposition.addressLocked(address), amount, Props.boxValue)

  def genTxOutputs(boxes: Traversable[EncryBaseBox]): IndexedSeq[ADKey] =
    boxes.foldLeft(IndexedSeq[ADKey]()) { case(s, box) =>
      s :+ box.id
    }
}
