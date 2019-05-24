package encry.modifiers.state.box.proposition

import org.encryfoundation.common.utils.TaggedTypes.Height .Height
import scorex.crypto.authds.ADDigest
import scorex.utils.Random

trait StateContext {

  val height: Height = Height @@ 1091
  val lastBlockTimestamp: Long = 12345678
  val stateDigest: ADDigest = ADDigest @@ Random.randomBytes()
}
