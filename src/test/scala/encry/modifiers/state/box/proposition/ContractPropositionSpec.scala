package encry.modifiers.state.box.proposition

import encry.crypto.{PrivateKey25519, PublicKey25519}
import encry.modifiers.InstanceFactory
import encry.modifiers.mempool.EncryTransaction
import encry.modifiers.state.box.Context
import encry.modifiers.state.box.proof.{MultiSig, Proof}
import encry.view.history.Height
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class ContractPropositionSpec extends PropSpec with Matchers with SmartContracts with StateContext with InstanceFactory {


  property("Unlocking proposition with dummy contract") {

    val defaultCtx: Context = Context(fakeTransaction, height, lastBlockTimestamp, stateDigest)

    val proposition: ContractProposition = ContractProposition(DummyContract)
    val fakeProof: Proof = fakeTransaction.defaultProofOpt.get

    val unlockR: Try[Unit] = proposition.unlockTry(fakeProof, defaultCtx)

    unlockR.isSuccess shouldBe true
  }

  property("Unlocking proposition with HeightLock contract (Should unlock)") {

    val ctx: Context = Context(fakeTransaction, Height @@ 1269, lastBlockTimestamp, stateDigest)

    val proposition: ContractProposition = ContractProposition(HLContract)
    val fakeProof: Proof = fakeTransaction.defaultProofOpt.get

    val unlockR: Try[Unit] = proposition.unlockTry(fakeProof, ctx)

    unlockR.isSuccess shouldBe true
  }

  property("Unlocking proposition with HeightLock contract (Should fail)") {

    val ctx: Context = Context(fakeTransaction, Height @@ 561, lastBlockTimestamp, stateDigest)

    val proposition: ContractProposition = ContractProposition(HLContract)
    val fakeProof: Proof = fakeTransaction.defaultProofOpt.get

    val unlockR: Try[Unit] = proposition.unlockTry(fakeProof, ctx)

    unlockR.isSuccess shouldBe false
  }

  property("Unlocking proposition with AccountLock contract (Should unlock)") {

    val ctx: Context = Context(TransactionForContract, Height @@ 561, lastBlockTimestamp, stateDigest)

    val proposition: ContractProposition = ContractProposition(ALContract)
    val proof: Proof = TransactionForContract.defaultProofOpt.get

    val unlockR: Try[Unit] = proposition.unlockTry(proof, ctx)

    unlockR.isSuccess shouldBe true
  }

  property("Unlocking proposition with AccountLock contract (Should fail - wrong signature)") {

    val ctx: Context = Context(TransactionForContract, Height @@ 561, lastBlockTimestamp, stateDigest)

    val proposition: ContractProposition = ContractProposition(ALContract)
    val proof: Proof = fakeTransaction.defaultProofOpt.get

    val unlockR: Try[Unit] = proposition.unlockTry(proof, ctx)

    unlockR.isSuccess shouldBe false
  }

  property("Unlocking proposition with AccountLock contract (Should unlock, complex contract)") {

    val ctx: Context = Context(TransactionForContract, Height @@ 561, lastBlockTimestamp, stateDigest)

    val proposition: ContractProposition = ContractProposition(ALContract)
    val proof: Proof = TransactionForContract.defaultProofOpt.get

    val unlockR: Try[Unit] = proposition.unlockTry(proof, ctx)

    unlockR.isSuccess shouldBe true
  }

  property("Unlocking proposition with multiSig contract") {

    val tx: EncryTransaction = TransactionForMultiSigContractWithMyToken

    val privKeys: Seq[PrivateKey25519] = secrets.slice(0, 3)

    val pubKeys: Seq[PublicKey25519] = privKeys.map(_.publicImage)

    val defaultCtx: Context = Context(tx, height, lastBlockTimestamp, stateDigest)

    val proposition: ContractProposition = ContractProposition(multiSigContract(pubKeys.head, pubKeys(1), pubKeys.last))

    val multiSigProof: MultiSig = MultiSig(
      privKeys.map(_.sign(EncryTransaction.getMessageToSign(tx.fee, tx.timestamp, tx.unlockers, tx.directives)))
    )

    val unlockR: Try[Unit] = proposition.unlockTry(multiSigProof, defaultCtx)

    unlockR.isSuccess shouldBe true
  }
}
