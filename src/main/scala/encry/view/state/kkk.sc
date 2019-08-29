import cats.Applicative
import cats.instances.list._
import cats.instances.future._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

def a[F[_]](transaction: Int)(implicit appl: Applicative[F]): F[Int] = {
  appl.pure(transaction)
}

a[List](1)

a[Future](2)