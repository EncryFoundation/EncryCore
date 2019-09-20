package encry.view.state.avlTree

import java.math.BigInteger

import encry.utils.EncryGenerator
import org.scalatest.{Matchers, PropSpec}

import scala.util.Random

class AvlTreeTest extends PropSpec with Matchers with EncryGenerator {

  property("dummy insert test"){

    import cats.instances.int._
    import cats.instances.string._

    val avl = AvlTree[Int, String]()

    val startTime = System.currentTimeMillis()

    val randomList = (0 until 10000).map(_ => Random.nextInt()).zipWithIndex

    val newAvl = randomList.foldLeft(AvlTree[Int, Int]()) {
      case (avl, i) =>
        val nanoTime = System.currentTimeMillis()
        val res = avl.insert(i._1, i._1)
        //println(s"Insert time${i._2}: ${(System.currentTimeMillis() - nanoTime)} ms")
        res
    }


    println(newAvl.get(1).isDefined)

  }
}
