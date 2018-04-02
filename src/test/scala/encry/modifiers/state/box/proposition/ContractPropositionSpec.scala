package encry.modifiers.state.box.proposition

import encry.modifiers.InstanceFactory
import encry.modifiers.state.box.Context
import encry.view.history.Height
import org.scalatest.{Matchers, PropSpec}

class ContractPropositionSpec extends PropSpec with Matchers with SmartContracts with StateContext {

  import InstanceFactory._

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
}
