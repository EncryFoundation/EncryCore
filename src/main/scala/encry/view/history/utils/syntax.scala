package encry.view.history.utils

import io.iohk.iodb.ByteArrayWrapper

object syntax {

  object wrapper {
    implicit class WrapperOps[T](val t: T) extends AnyVal {
      def wrap(implicit wrapper: Wrapper[T]): ByteArrayWrapper = wrapper.wrap(t)
    }
    implicit class UnWrapperOps(val t: ByteArrayWrapper) extends AnyVal {
      def unwrap(implicit wrapper: Wrapper[ByteArrayWrapper]): ByteArrayWrapper = wrapper.unwrap(t)
    }
  }

}
