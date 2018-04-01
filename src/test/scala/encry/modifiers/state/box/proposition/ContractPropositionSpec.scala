package encry.modifiers.state.box.proposition

import encry.modifiers.InstanceFactory
import encry.modifiers.state.box.Context
import encry.view.history.Height
import org.scalatest.{Matchers, PropSpec}

class ContractPropositionSpec extends PropSpec with Matchers with SmartContracts with StateContext {

  property("Unlocking proposition with dummy contract") {

    // TODO: Use real case transaction for more complex testing.
    implicit val defaultCtx: Context = Context(InstanceFactory.paymentTransactionValid, height, lastBlockTimestamp, stateDigest)

    val proposition = ContractProposition(DummyContract)
    val fakeProof = InstanceFactory.paymentTransactionValid.signature

    val unlockR = proposition.unlockTry(fakeProof)

    unlockR.isSuccess shouldBe true
  }

  property("Unlocking proposition with HeightLock contract (Should unlock)") {

    implicit val ctx: Context = Context(InstanceFactory.paymentTransactionValid, Height @@ 1269, lastBlockTimestamp, stateDigest)

    val proposition = ContractProposition(HLContract)
    val fakeProof = InstanceFactory.paymentTransactionValid.signature

    val unlockR = proposition.unlockTry(fakeProof)

    unlockR.isSuccess shouldBe true
  }

  property("Unlocking proposition with HeightLock contract (Should fail)") {

    implicit val ctx: Context = Context(InstanceFactory.paymentTransactionValid, Height @@ 561, lastBlockTimestamp, stateDigest)

    val proposition = ContractProposition(HLContract)
    val fakeProof = InstanceFactory.paymentTransactionValid.signature

    val unlockR = proposition.unlockTry(fakeProof)

    unlockR.isSuccess shouldBe false
  }
}
