package encry.modifiers

import encry.local.TestHelper
import encry.modifiers.mempool._
import encry.modifiers.mempool.directive.ScriptedAssetDirective
import encry.modifiers.state.Keys
import encry.modifiers.state.box.AssetBox
import encry.modifiers.state.box.proof.Signature25519
import encry.modifiers.state.box.proposition.{AccountProposition, OpenProposition, SmartContracts}
import encry.view.history.Height
import scorex.crypto.signatures.Curve25519

import scala.util.{Random => Scarand}

trait InstanceFactory extends SmartContracts with Keys{

  private val genHelper = TestHelper
  private val timestamp = System.currentTimeMillis()

  lazy val fakeTransaction: EncryTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address),
      genHelper.genAssetBox(publicKey.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, timestamp, useBoxes,
      publicKey.address, 12345678L)
  }

  lazy val paymentTransactionValid: EncryTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address),
      genHelper.genAssetBox(publicKey.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, timestamp, useBoxes,
      publicKey.address, genHelper.Props.txAmount)
  }


  def paymentTransactionValidWithSmartContractDirectives: EncryTransaction = {

    val pubKey = secret.publicImage
    val fee = Scarand.nextLong()
    val timestamp = Scarand.nextLong()
    val amount = Scarand.nextLong()
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address, amount + fee + Scarand.nextLong()))
    val unlockers = useBoxes.map(bx => Unlocker(bx.id, None)).toIndexedSeq
    val change = useBoxes.map(_.amount).sum - (amount + fee)
    val dirAmount = amount / 4
    val directives = if(change > 0) {
      IndexedSeq(
        ScriptedAssetDirective(DummyContract, dirAmount, 0),
        ScriptedAssetDirective(HLContract, dirAmount, 0),
        ScriptedAssetDirective(ALContract, dirAmount, 0),
        ScriptedAssetDirective(ALContract2, dirAmount, 0)
      )
    } else {
      IndexedSeq(ScriptedAssetDirective(DummyContract, dirAmount, 0))
    }

    val msg = EncryTransaction.getMessageToSign(
      pubKey,
      fee,
      timestamp,
      unlockers,
      directives
    )

    val sig = new Signature25519(Curve25519.sign(secret.privKeyBytes, msg))

    EncryTransaction(
      pubKey,
      fee,
      timestamp,
      sig,
      unlockers,
      directives
    )
  }

  def paymentTransactionDynamic: EncryTransaction = {
    val fee = genHelper.Props.txFee
    val useBoxes = (0 to 5).map(_ => {
      AssetBox(
        AccountProposition(secret.publicImage.address),
        Scarand.nextLong(),
        999L
      )
    })

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, Scarand.nextLong(), useBoxes,
      publicKey.address, genHelper.Props.txAmount)
  }

  lazy val paymentTransactionInvalid: EncryTransaction = {
    val useBoxes = IndexedSeq(genHelper.genAssetBox(publicKey.address))

    TransactionFactory.defaultPaymentTransactionScratch(secret, -100, timestamp, useBoxes,
      genHelper.Props.recipientAddr, genHelper.Props.txAmount)
  }

  lazy val coinbaseTransaction: EncryTransaction = {
    val useBoxes = IndexedSeq(genHelper.genAssetBox(secret.publicImage.address))
    TransactionFactory.coinbaseTransactionScratch(secret, timestamp, useBoxes, Height @@ 0)
  }

  lazy val assetBox: AssetBox =
    AssetBox(
      AccountProposition(secret.publicImage.address),
      999L,
      100000L
    )

  lazy val openAssetBox: AssetBox =
    AssetBox(
      OpenProposition,
      999L,
      100000L
    )
}
