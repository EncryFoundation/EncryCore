package encry.modifiers.state.box.proposition

import encry.modifiers.InstanceFactory
import encry.modifiers.mempool.EncryTransaction
import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.MultiSig
import encry.view.history.Height
import org.scalatest.{Matchers, PropSpec}

class ContractPropositionSpec extends PropSpec with Matchers with SmartContracts with StateContext with InstanceFactory {


  property("Unlocking proposition with dummy contract") {

    implicit val defaultCtx: Context = Context(fakeTransaction, height, lastBlockTimestamp, stateDigest)

    val proposition = ContractProposition(DummyContract)
    val fakeProof = fakeTransaction.signature

    val unlockR = proposition.unlockTry(fakeProof)

    unlockR.isSuccess shouldBe true
  }

  property("Unlocking proposition with HeightLock contract (Should unlock)") {

    implicit val ctx: Context = Context(fakeTransaction, Height @@ 1269, lastBlockTimestamp, stateDigest)

    val proposition = ContractProposition(HLContract)
    val fakeProof = fakeTransaction.signature

    val unlockR = proposition.unlockTry(fakeProof)

    unlockR.isSuccess shouldBe true
  }

  property("Unlocking proposition with HeightLock contract (Should fail)") {

    implicit val ctx: Context = Context(fakeTransaction, Height @@ 561, lastBlockTimestamp, stateDigest)

    val proposition = ContractProposition(HLContract)
    val fakeProof = fakeTransaction.signature

    val unlockR = proposition.unlockTry(fakeProof)

    unlockR.isSuccess shouldBe false
  }

  property("Unlocking proposition with AccountLock contract (Should unlock)") {

    implicit val ctx: Context = Context(TransactionForContract, Height @@ 561, lastBlockTimestamp, stateDigest)

    val proposition = ContractProposition(ALContract)
    val proof = TransactionForContract.signature

    val unlockR = proposition.unlockTry(proof)

    unlockR.isSuccess shouldBe true
  }

  property("Unlocking proposition with AccountLock contract (Should fail - wrong signature)") {

    implicit val ctx: Context = Context(TransactionForContract, Height @@ 561, lastBlockTimestamp, stateDigest)

    val proposition = ContractProposition(ALContract)
    val proof = fakeTransaction.signature

    val unlockR = proposition.unlockTry(proof)

    unlockR.isSuccess shouldBe true
  }

  property("Unlocking proposition with AccountLock contract (Should unlock, complex contract)") {

    implicit val ctx: Context = Context(TransactionForContract, Height @@ 561, lastBlockTimestamp, stateDigest)

    val proposition = ContractProposition(ALContract2)
    val proof = TransactionForContract.signature

    val unlockR = proposition.unlockTry(proof)

    unlockR.isSuccess shouldBe true
  }

  property("Unlocking proposition with multiSig contract") {

    val tx = TransactionForMultiSigContractWithMyToken

    val privKeys = secrets.slice(0, 3)

    val pubKeys = privKeys.map(_.publicImage)

    implicit val defaultCtx: Context = Context(tx, height, lastBlockTimestamp, stateDigest)

    val proposition = ContractProposition(multiSigContract(pubKeys.head, pubKeys(1), pubKeys.last))

    val multiSigProof = MultiSig(
      privKeys.map(_.sign(EncryTransaction.getMessageToSign(tx.accountPubKey, tx.fee, tx.timestamp, tx.directives)))
    )

    val unlockR = proposition.unlockTry(multiSigProof)

    unlockR.isSuccess shouldBe true
  }
}
