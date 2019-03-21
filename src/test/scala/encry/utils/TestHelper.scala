package encry.utils

import java.io.File

import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.{AssetBox, EncryBaseBox, EncryProposition}
import encry.settings.{EncryAppSettings, NodeSettings}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.view.history.EncryHistory
import encry.view.history.processors.payload.BlockPayloadProcessor
import encry.view.history.processors.proofs.FullStateProofProcessor
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.transaction.EncryAddress
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.iq80.leveldb.Options
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
