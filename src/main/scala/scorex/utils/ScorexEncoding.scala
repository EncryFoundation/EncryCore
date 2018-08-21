package scorex.utils

import scorex.crypto.encode.{Base16, BytesEncoder}

/**
  * Trait with bytes to string encoder
  * TODO extract to ScorexUtils
  */
trait ScorexEncoding {
  implicit val encoder: BytesEncoder = Base16
}
