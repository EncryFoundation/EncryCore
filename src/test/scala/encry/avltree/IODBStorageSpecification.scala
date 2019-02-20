package encry.avltree

import com.typesafe.scalalogging.StrictLogging
import encry.avltree.helpers.TestHelper
import encry.storage.levelDb.versionalLevelDB.{LevelDbElem, VersionalLevelDB}
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, VersionalLevelDbValue}
import org.encryfoundation.common.Algos
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.{Blake2b256, Digest32}

import scala.collection.mutable.ArrayBuffer

class IODBStorageSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TestHelper
  with StrictLogging {

  override protected val KL = 32
  override protected val VL = 8
  override protected val LL = 32

  val storeTest: VersionalLevelDB => Unit = { store =>
    var version = store.currentVersion
    logger.info(s"Current store version: ${Algos.encode(version)}")
    val keys: ArrayBuffer[(VersionalLevelDbKey, VersionalLevelDbValue)] = ArrayBuffer()
    forAll { input: String =>
      val pair = (VersionalLevelDbKey @@ Blake2b256(input).untag(Digest32),
        VersionalLevelDbValue @@ Blake2b256(input).untag(Digest32))
      keys += pair
      val nextVersion = LevelDBVersion @@ Blake2b256(version).untag(Digest32)
      store.insert(
        LevelDbElem(
          nextVersion,
          List(pair)
        )
      )
      store.rollbackTo(version)
      store.insert(
        LevelDbElem(
          nextVersion,
          List(pair)
        )
      )
      version = nextVersion
      keys.foreach(k => {
        store.get(k._1).get shouldEqual k._2
      })
    }
  }

  property("VLDB") { storeTest(createVLDB(11, 32)) }
}
