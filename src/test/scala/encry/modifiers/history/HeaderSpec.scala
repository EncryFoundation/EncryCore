package encry.modifiers.history

import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.Blake2b256
import encry.utils.TestEntityGenerator.genHeader

class HeaderSpec extends PropSpec with Matchers {

  property("Different headers should have different hash") {

    val header = genHeader

    val initialId = header.id

    header.copy(version = (header.version + 1).toByte).id should not equal initialId
    header.copy(parentId = initialId).id should not equal initialId
    header.copy(transactionsRoot = Blake2b256(header.transactionsRoot)).id should not equal initialId
    header.copy(timestamp = header.timestamp + 1).id should not equal initialId
    header.copy(height = header.height + 1).id should not equal initialId
    header.copy(nonce = header.nonce + 1).id should not equal initialId

  }

}
