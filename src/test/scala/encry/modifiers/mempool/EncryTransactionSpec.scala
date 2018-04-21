package encry.modifiers.mempool

import encry.modifiers.InstanceFactory
import org.scalatest.{Matchers, PropSpec}

class EncryTransactionSpec extends PropSpec with Matchers with InstanceFactory {

  private val txValid = paymentTransactionValid

  private val txInvalid = paymentTransactionInvalid

  property("semanticValidity of valid tx") {

    txValid.semanticValidity.isSuccess shouldBe true
  }

  property("semanticValidity of invalid tx") {

    txInvalid.semanticValidity.isSuccess shouldBe false
  }
}
