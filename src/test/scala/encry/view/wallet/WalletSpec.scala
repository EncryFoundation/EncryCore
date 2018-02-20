package encry.view.wallet

import java.io.File

import encry.account.Address
import encry.consensus.Difficulty
import encry.local.TestHelper
import encry.modifiers.InstanceFactory
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.PaymentTransaction
import encry.modifiers.state.box.AssetBox

import scala.concurrent.duration._
import encry.settings.{Algos, Constants, EncryAppSettings}
import encry.utils.FileHelper
import encry.view.state.BoxHolder
import encry.view.wallet.keys.KeyManager
import io.iohk.iodb.LSMStore
import org.scalatest.{Matchers, PropSpec}
import scorex.core.ModifierId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{PublicKey, Signature}
import scorex.utils.Random

import scala.concurrent.Await

class WalletSpec extends PropSpec with Matchers{

  property("Balance count"){

    val blockHeader = EncryBlockHeader(
      99: Byte,
      new PublicKey25519Proposition(PublicKey @@ Random.randomBytes()),
      Signature25519(Signature @@ Random.randomBytes(64)),
      ModifierId @@ Random.randomBytes(),
      Digest32 @@ Random.randomBytes(),
      ADDigest @@ Random.randomBytes(33),
      Digest32 @@ Random.randomBytes(),
      99999L,
      199,
      999L,
      Difficulty @@ BigInt(999999999999999L)
    )

    lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(Option(""))

    val walletStore = new LSMStore(FileHelper.getRandomTempDir, keepVersions = Constants.keepVersions)

    val keyManager = KeyManager(new LSMStore(FileHelper.getRandomTempDir, keepVersions = Constants.keepVersions), encrySettings.keyManagerSettings, None)

    keyManager.initStorage(Random.randomBytes())

    val wallet = new EncryWallet(walletStore, keyManager)

    val bxs = TestHelper.genAssetBoxes

    val bh = BoxHolder(bxs)

    val factory = TestHelper
    val keys = factory.getOrGenerateKeys(factory.Props.keysFilePath)

    val validTxs = keys.zip(bxs).map { case (pk, bx) =>
      val proposition = pk.publicImage
      val fee = factory.Props.txFee
      val timestamp = 1234567L
      val useBoxes = IndexedSeq(bx).map(_.id)
      val outputs = IndexedSeq((Address @@ wallet.publicKeys.head.address, factory.Props.boxValue - 100))
      val sig = PrivateKey25519Companion.sign(
        pk,
        PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
      )
      PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
    }.slice(0, 4)

    val correctBalance = validTxs.foldLeft(0L){
      case(sum, tx) => sum + tx.newBoxes.foldLeft(0L){
        case(boxesSum, box) =>
          val boxAmount = box match {
            case box: AssetBox => box.amount
            case _ => 0L
          }
          boxesSum + boxAmount
      }
    }

    val blockPayload = new EncryBlockPayload(ModifierId @@ Array.fill(32)(19: Byte), validTxs.slice(0, 4) :+ InstanceFactory.addPubKeyInfoTransaction())

    val adProofs = ADProofs(ModifierId @@ Random.randomBytes(), SerializedAdProof @@ Random.randomBytes())

    val block = new EncryBlock(blockHeader,blockPayload,Option(adProofs))

    wallet.scanPersistent(block)

    wallet.balance shouldBe correctBalance

  }


}
