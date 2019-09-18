package encry.view.state.avlTree

import encry.utils.EncryGenerator
import org.scalatest.{Matchers, PropSpec}

class AvlTreeTest extends PropSpec with Matchers with EncryGenerator {

  property("dummy insert test"){

    import cats.instances.int._
    import cats.instances.string._

    val avl = AvlTree[Int, String]()

    val newAvl = avl
      .insert(3, "test")
      .insert(4, "test123")
      .insert(5, "sss")

    println(newAvl)
  }
}
