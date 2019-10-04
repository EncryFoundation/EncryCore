package encry.view.state.avlTree

import com.google.common.primitives.Ints
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.{EncryGenerator, FileHelper}
import encry.view.state.ChunksCreator
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, PropSpec}
import org.specs2.mutable.Specification
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._
import io.iohk.iodb.ByteArrayWrapper
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging

import scala.collection.immutable
import scala.collection.immutable.HashMap


class SubTreeTests extends PropSpec with Matchers with EncryGenerator with StrictLogging {

//  property("qwerty") {
//    val dir = FileHelper.getRandomTempDir
//
//    val storage = {
//      val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
//      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
//    }
//
//    val avlStorage = AvlTree[Int, Int](storage)
//
//    val dir1 = FileHelper.getRandomTempDir
//
//    val storage1 = {
//      val levelDBInit = LevelDbFactory.factory.open(dir1, new Options)
//      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
//    }
//
//    val avlStorage1 = AvlTree[Int, Int](storage1)
//
//    val list: immutable.Seq[Int] = (0 until 100).map(i => i)
//
//    val res1: AvlTree[Int, Int] = list.foldLeft(avlStorage) { case (avl, el) =>
//      logger.info(s"========= START el $el ========")
//      logger.info(s"Inserting: $el.")
//      val k = avl.fastSyncInsertion(List(el -> el))
//      logger.info(s"After insert $el new root ${k.root}")
//      logger.info(s"RootNode: ${k.rootNode}")
//      logger.info(s"========= FINISH el $el ========\n\n")
//      k
//    }
//
//    val res2: AvlTree[Int, Int] = list.reverse.foldLeft(avlStorage1) { case (avl, el) =>
//      logger.info(s"========= START el $el ========")
//      logger.info(s"Inserting: $el.")
//      val k = avl.fastSyncInsertion(List(el -> el))
//      logger.info(s"After insert $el new root ${k.root}")
//      logger.info(s"RootNode: ${k.rootNode}")
//      logger.info(s"========= FINISH el $el ========\n")
//      k
//    }
//
//    logger.info(s"Res1 = ${res1.root} --> Res2 = ${res2.root}")
//    logger.info(s"Res1 = ${res1.rootNode}")
//    logger.info(s"Res2 = ${res2.rootNode}")
//    res1.root == res2.root shouldBe true
//
//  }

  property("create subtree correctly") {

    import encry.utils.implicits.UTXO._

    val dir = FileHelper.getRandomTempDir

    val storage = {
      val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }

    val avlStorage = AvlTree[StorageKey, StorageValue](storage)

    val interval = 50

    val boxes = (0 to interval).map { i =>
      val addr = "9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia"
      genAssetBox(addr, i, nonce = i)
    }.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

    //println(boxes.map(bx => Algos.encode(bx._1)).mkString("\n "))


    val newAvl: AvlTree[StorageKey, StorageValue] = avlStorage.insertAndDeleteMany(
      StorageVersion @@ Random.randomBytes(),
      boxes.toList,
      List.empty
    )
    val newAvlRoot = newAvl.root

    val startTime = System.currentTimeMillis()
    println("start calculating")
    val chunks: List[ChunksCreator.TmpStateChunk] = newAvl.createSubtrees(List(newAvl.rootNode), List.empty)
    val retChunks = newAvl.retirnSubtrees(chunks)
    println(s"Finished ${(System.currentTimeMillis() - startTime) / 1000} sec. Chunks size: ${chunks.size}")
    chunks.nonEmpty shouldBe true

    println(s"old root: ${newAvl.rootNode}")


    val dir2 = FileHelper.getRandomTempDir

    val storage2 = {
      val levelDBInit = LevelDbFactory.factory.open(dir2, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }

    val avlStorage2 = AvlTree[StorageKey, StorageValue](storage2)

    val jdf: AvlTree[StorageKey, StorageValue] = newAvl.assembleTree(avlStorage2, chunks)

    println(s"${Algos.encode(jdf.root)} -->>> ${Algos.encode(newAvlRoot)}")
    println(s"======RESULT======\n")
    println(s"${newAvl.rootNode}")
    println(s"=====================================\n")
    println(s"${jdf.rootNode}")
    println(s"======END======\n")

    val ert = retChunks.forall(ch =>
      jdf.contains(ch.key)
    )

    retChunks.foreach { n =>
      if (!jdf.contains(n.key)) {
        println(s"\n\nNo key ${Algos.encode(n.key.asInstanceOf[StorageKey])}")
        println(s"But root key is: ${Algos.encode(jdf.rootNode.key.asInstanceOf[StorageKey])}\n\n")

        println(s"\n\nNo key ${n.key}")
        println(s"But root key is: ${jdf.rootNode.key}\n\n")

        Algos.encode(n.key.asInstanceOf[StorageKey]) shouldBe Algos.encode(jdf.rootNode.key.asInstanceOf[StorageKey])

        val rt = jdf.get(n.key)
        println(s"No element ${n}")
        println(rt)
      }

    }

    ert shouldBe true

    jdf.root.sameElements(newAvlRoot) shouldBe true
  }

}
