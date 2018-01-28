package encry.modifiers.mempool

import org.scalatest.{Matchers, PropSpec}

class PaymentTransactionSpec extends PropSpec with Matchers {

  private val txValid = InstanceFactory.paymentTransactionValid

  private val txInvalid = InstanceFactory.paymentTransactionInvalid

  property("semanticValidity of valid tx") {

    val checkValidityTry = txValid.semanticValidity

    checkValidityTry.isSuccess shouldBe true
  }

  property("semanticValidity of invalid tx") {

    val checkValidityTry = txInvalid.semanticValidity

    checkValidityTry.isSuccess shouldBe false
  }
}
