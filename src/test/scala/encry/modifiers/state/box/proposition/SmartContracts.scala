package encry.modifiers.state.box.proposition

import encry.crypto.PublicKey25519
import encry.modifiers.mempool.{EncryTransaction, TransactionFactory}
import encry.modifiers.state.Keys
import encry.modifiers.state.box.AssetBox
import encry.settings.Algos
import encrywm.common.{EncryContract, SourceProcessor}
import scorex.crypto.authds.ADKey
import scorex.crypto.encode.Base58

trait SmartContracts extends Keys {

  val DummyContract: EncryContract = {
    val source =
      """
        |unlock if true
      """.stripMargin

    SourceProcessor.source2Contract(source).get
  }

  // Height lock
  val HLContract: EncryContract = {
    val source =
      """
        |let unlockStart = 1000
        |let unlockFinish = 2000
        |unlock if context.state.height >= unlockStart and context.state.height <= unlockFinish
      """.stripMargin

    SourceProcessor.source2Contract(source).get
  }

  // Account lock
  val ALContract: EncryContract = {
    val source =
      s"""
        |let ownerPubKey = base58"${Base58.encode(publicKey.pubKeyBytes)}"
        |unlock if checkSig(context.transaction.signature, context.transaction.messageToSign, ownerPubKey)
      """.stripMargin

    SourceProcessor.source2Contract(source).get
  }

  val ALContract2: EncryContract = {
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

  def multiSigContract(pubKey1: PublicKey25519, pubKey2: PublicKey25519, pubKey3: PublicKey25519): EncryContract = {
    val source =
      s"""
         |let minobrPk = base58'$pubKey1'
         |let universityPk = base58'$pubKey2'
         |let studentPk = base58'$pubKey3'
         |
         |let myAssetId = base58'GtBn7qJwK1v1EbB6CZdgmkcvt849VKVfWoJBMEWsvTew'
         |
         |def isMyAsset(box: Box) -> Bool:
         |    match box:
         |        case asset -> AssetBox:
         |            return asset.tokenIdOpt.isDefined && asset.tokenIdOpt.get == myAssetId
         |        case _:
         |            return false
         |
         |def validTresholdSig(proof: Proof) -> Bool:
         |    match proof:
         |        case mulp -> MultiSig:
         |            let sig1 = 1 if mulp.proofs[0].isDefined && checkSig(mulp.proofs[0].get.sigBytes, context.transaction.messageToSign, minobrPk) else 0
         |            let sig2 = 1 if mulp.proofs[1].isDefined && checkSig(mulp.proofs[1].get.sigBytes, context.transaction.messageToSign, universityPk) else 0
         |            let sig3 = 1 if mulp.proofs[2].isDefined && checkSig(mulp.proofs[2].get.sigBytes, context.transaction.messageToSign, studentPk) else 0
         |            return (sig1 + sig2 + sig3) >= 2
         |        case _:
         |            return false
         |
         |unlock if validTresholdSig(context.proof) || context.transaction.outputs.exists(isMyAsset)
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

  val TransactionForMultiSigContractWithMyToken: EncryTransaction = {
    val boxWithMyToken = AssetBox(
      ContractProposition(DummyContract),
      12345L,
      99999L,
      Option(ADKey @@ Algos.decode("GtBn7qJwK1v1EbB6CZdgmkcvt849VKVfWoJBMEWsvTew").get))
    val fee = 99
    val useBoxes = IndexedSeq(boxWithMyToken)
    TransactionFactory.defaultPaymentTransactionScratch(secret, fee, 123456789L, useBoxes,
      publicKey.address, 12345678L)
  }
}
