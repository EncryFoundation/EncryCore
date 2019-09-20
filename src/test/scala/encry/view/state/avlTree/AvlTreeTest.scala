package encry.view.state.avlTree

import cats.Order
import encry.utils.EncryGenerator
import org.scalatest.{Matchers, PropSpec}
import cats.instances.int._
import cats.instances.string._
import encry.view.state.avlTree.utils.implicits.Hashable
import org.encryfoundation.common.utils.Algos

import scala.util.Random

class AvlTreeTest extends PropSpec with Matchers with EncryGenerator {

  implicit val hashInt: Hashable[Int] = new Hashable[Int] {
    override def hash(value: Int): Array[Byte] = Algos.hash(value.toString)
  }

  property("avl should contains all inserted elems"){

    val avl = AvlTree[Int, Int]()

    val elems = (0 to 100).map(_ => Random.nextInt())

    val newAvl = elems.foldLeft(avl){
      case (prevTree, elemToInsert) => prevTree.insert(elemToInsert, elemToInsert)
    }

    elems.forall(newAvl.contains) shouldBe true
  }

  property("avl should shouldn't contains deleted elems"){

    val avl = AvlTree[Int, Int]()

    val elems = (0 to 100).map(_ => Random.nextInt())

    val newAvl = elems.foldLeft(avl){
      case (prevTree, elemToInsert) => prevTree.insert(elemToInsert, elemToInsert)
    }

    val toDelete = elems.take(Random.nextInt(elems.length - 1))

    val avlAfterDeletions = toDelete.foldLeft(newAvl){ case (prevTree, elemToDelete) => prevTree.delete(elemToDelete)}

    toDelete.forall { elem => !avlAfterDeletions.contains(elem) } shouldBe true
    elems.diff(toDelete).forall(avlAfterDeletions.contains) shouldBe true
  }
}
