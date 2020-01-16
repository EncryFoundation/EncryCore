package encry.view.history.utils

import io.iohk.iodb.ByteArrayWrapper

trait Wrapper[T] {
  def wrap(t: T): ByteArrayWrapper
  def unwrap(b: ByteArrayWrapper): T
}
