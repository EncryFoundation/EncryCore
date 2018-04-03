package encry.modifiers.state.box.proposition

import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.Keys
import encry.modifiers.state.box.AssetBox
import encrywm.common.{ESContract, SourceProcessor}
import scorex.crypto.encode.Base58

trait SmartContracts extends Keys {

  val DummyContract: ESContract = {
    val source =
      """
        |unlock if true
      """.stripMargin

    SourceProcessor.source2Contract(source).get
  }

  // Height lock
  val HLContract: ESContract = {
    val source =
      """
        |let unlockStart = 1000
        |let unlockFinish = 2000
        |unlock if context.state.height >= unlockStart and context.state.height <= unlockFinish
      """.stripMargin

    SourceProcessor.source2Contract(source).get
  }

  // Account lock
  val ALContract: ESContract = {
    val source =
      s"""
        |let ownerPubKey = base58"${Base58.encode(publicKey.pubKeyBytes)}"
        |unlock if checkSig(context.transaction.signature, context.transaction.messageToSign, ownerPubKey)
      """.stripMargin

    SourceProcessor.source2Contract(source).get
  }

  val ALContract2: ESContract = {
    val source =
      s"""
         |let ownerPubKey = base58"${Base58.encode(publicKey.pubKeyBytes)}"
         |match context.proof:
         |    case sig -> Signature25519:
         |        unlock if checkSig(sig.sigBytes, context.transaction.messageToSign, ownerPubKey)
         |    case _:
         |        abort
      """.stripMargin

    SourceProcessor.source2Contract(source).get
  }

  val TransactionForContract: EncryTransaction = {
    val box = AssetBox(ContractProposition(ALContract), 12345L, 99999L)
    val fee = 99L
    val useBoxes = IndexedSeq(box)

    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, 123456789L, useBoxes,
      publicKey.address, 12345678L)
  }
}
