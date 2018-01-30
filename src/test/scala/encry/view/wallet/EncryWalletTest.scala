package encry.view.wallet

import encry.account.Address
import encry.consensus.Difficulty
import encry.local.TestHelper
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.PaymentTransaction
import encry.settings.EncryAppSettings
import org.scalatest.FunSuite
import scorex.core.ModifierId
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.core.transaction.state.PrivateKey25519Companion
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.{PublicKey, Signature}
import scorex.utils.Random

class EncryWalletTest extends FunSuite {

  test("Encry Wallet test"){

    lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(Option(""))

    var wallet: EncryWallet = EncryWallet.readOrGenerate(encrySettings)

    wallet.keyManager.initStorage("testSeed".getBytes())

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

    val factory = TestHelper
    val keys = factory.getOrGenerateKeys(factory.Props.keysFilePath)

    val validTxs = keys.map { key =>
      val proposition = key.publicImage
      val fee = factory.Props.txFee
      val timestamp = 1234567L
      val useBoxes = IndexedSeq(factory.genAssetBox(Address @@ key.publicImage.address)).map(_.id)
      val outputs = IndexedSeq((Address @@ wallet.keyManager.keys.head.publicImage.address, factory.Props.boxValue))
      val sig = PrivateKey25519Companion.sign(
        key,
        PaymentTransaction.getMessageToSign(proposition, fee, timestamp, useBoxes, outputs)
      )
      PaymentTransaction(proposition, fee, timestamp, sig, useBoxes, outputs)
    }

    val trxCount = 50

    val blockPayload = new EncryBlockPayload(ModifierId @@ Array.fill(32)(19: Byte), validTxs.slice(0,trxCount))

    val adProofs = ADProofs(ModifierId @@ Random.randomBytes(), SerializedAdProof @@ Random.randomBytes())

    val block = new EncryBlock(blockHeader,blockPayload,Option(adProofs))

    wallet = wallet.scanPersistent(block)

    assert(trxCount*factory.Props.boxValue == wallet.balance, "Balance not equals")
  }
}
