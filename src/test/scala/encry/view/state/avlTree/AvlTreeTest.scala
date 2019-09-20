package encry.view.state.avlTree

import java.math.BigInteger

import cats.Order
import encry.utils.EncryGenerator
import org.scalatest.{Matchers, PropSpec}
import cats.instances.int._

import scala.util.Random

class AvlTreeTest extends PropSpec with Matchers with EncryGenerator {

  property("dummy insert test"){

    import cats.instances.int._
    import cats.instances.string._

    val avl = AvlTree[Int, String]()

    val orderImpl = implicitly[Order[Int]]

    println(orderImpl.compare(1, 4))

    val newAvl = avl
      .insert(1, "test")
      .insert(2, "test")
      .insert(3, "test")

    println(newAvl)

    println("=======")

  }
}
