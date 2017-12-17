package encry.utils

import scala.concurrent.{Future, Promise}
import scala.util.Success


trait CancellableStatus {

  def isCancelled: Boolean

  def nonCancelled: Boolean = !isCancelled
}

trait Cancellable {

  def cancel(): Boolean

  def status: CancellableStatus
}

object Cancellable {
  def apply(): Cancellable = new Cancellable {
    val promise: Promise[Unit] = Promise[Unit]()

    override def cancel(): Boolean = promise.tryComplete(Success(()))

    val status: CancellableStatus = new CancellableStatus {
      override def isCancelled: Boolean = promise.future.value.isDefined
    }
  }

  def run()(cont: CancellableStatus => Future[Unit]): Cancellable = {
    val cancellable = Cancellable()
    cont(cancellable.status) // run continuation feeding status
    cancellable
  }
}
