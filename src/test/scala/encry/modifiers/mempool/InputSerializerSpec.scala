package encry.modifiers.mempool

import encry.modifiers.InstanceFactory
import org.scalatest.{Matchers, PropSpec}

class InputSerializerSpec extends PropSpec with Matchers with InstanceFactory {

  property("toBytes/parseBytes") {

    val uInputSerialised = UnsignedInput.bytes

    val uInputDeserialisedTry = InputSerializer.parseBytes(uInputSerialised)

    uInputDeserialisedTry.isSuccess shouldBe true

    uInputDeserialisedTry.get.bytes sameElements UnsignedInput.bytes shouldBe true
  }
}
