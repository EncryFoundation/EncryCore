package encry.view.state.avlTree

import com.google.common.primitives.Ints
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.{EncryGenerator, FileHelper}
import encry.view.state.ChunksCreator
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, Height}
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, PropSpec}
import org.specs2.mutable.Specification
import scorex.utils.Random
import encry.view.state.avlTree.utils.implicits.Instances._
import io.iohk.iodb.ByteArrayWrapper
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.InstanceFactory
import encry.view.actors.SnapshotHolder
import org.encryfoundation.common.modifiers.history.{Block, Header}

import scala.collection.immutable
import scala.collection.immutable.HashMap


class SubTreeTests extends PropSpec with Matchers with InstanceFactory with StrictLogging {

  property("create subtree correctly") {

    import encry.utils.implicits.UTXO._

    val dir = FileHelper.getRandomTempDir
    val storage = {
      val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }
    val avlStorage = AvlTree[StorageKey, StorageValue](storage)
    val interval = 200
    val boxes = (0 to interval).map { i =>
      val addr = "9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia"
      genAssetBox(addr, i, nonce = i)
    }.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes))

    val newAvl: AvlTree[StorageKey, StorageValue] = avlStorage.insertAndDeleteMany(
      StorageVersion @@ Random.randomBytes(),
      boxes.toList,
      List.empty
    )

    val block = generateGenesisBlock(Height @@ 1)
    val startTime = System.currentTimeMillis()
    println("start calculating")
    val newInfo: (SnapshotHolder.SnapshotManifest, List[SnapshotHolder.SnapshotChunk]) = newAvl.initializeSnapshotData(block)
    println(s"Finished ${(System.currentTimeMillis() - startTime) / 1000} sec. Chunks size: ${newInfo._2.size}")
    newInfo._2.nonEmpty shouldBe true
    val newAvlRoot = newAvl.root

    val retChunks = newAvl.retirnSubtrees(newInfo._2)

    val dir2 = FileHelper.getRandomTempDir

    val storage2 = {
      val levelDBInit = LevelDbFactory.factory.open(dir2, new Options)
      VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
    }

    val avlStorage2 = AvlTree[StorageKey, StorageValue](newInfo._1, storage2)

    val jdf: AvlTree[StorageKey, StorageValue] = newAvl.assembleTree(avlStorage2, newInfo._2)

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
