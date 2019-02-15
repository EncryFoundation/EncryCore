package encry.storage.levelDb.versionalLevelDB

import encry.settings.LevelDBSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.VersionalLevelDbKey
import encry.utils.FileHelper
import encry.utils.levelDBUtils.LevelDbUnitsGenerator
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.Algos
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, PropSpec}

class VersionalLevelDBTest extends PropSpec with Matchers with LevelDbUnitsGenerator {

  /**
    * Check correctness of data stored in level db, after close and init.
    * Check correctness of version and elems, stored in level db
    */
  property("LevelDB should recover to last version") {

    val dummyLevelDBSettings: LevelDBSettings = LevelDBSettings(5)

    val tempDir = FileHelper.getRandomTempDir

    val levelDBInit = LevelDbFactory.factory.open(tempDir, new Options)

    val vldbInit = VersionalLevelDBCompanion(levelDBInit, dummyLevelDBSettings)

    val levelDbElems = generateRandomLevelDbElemsWithoutDeletions(3, 3)

    val keysToInsert: Seq[VersionalLevelDbKey] = levelDbElems.flatMap(_.elemsToInsert.map(_._1))

    val valuesHashes: List[ByteArrayWrapper] =
      levelDbElems.flatMap(_.elemsToInsert.map(elem => new ByteArrayWrapper(Algos.hash(elem._2.data))))

    levelDbElems.foreach(vldbInit.insert)

    vldbInit.close()

    val reopenedLevelDb = LevelDbFactory.factory.open(tempDir, new Options)

    val reopendVLDB = VersionalLevelDB(reopenedLevelDb, dummyLevelDBSettings)

    //Check correctness of version

    reopendVLDB.currentVersion shouldEqual levelDbElems.last.version

    //Check correctness of keys

    reopendVLDB.getAll.forall{case (elemKey, _) => keysToInsert.contains(elemKey)} shouldBe true

    //Check correctness of values

    reopendVLDB.getAll
      .forall{case (_, elemValue) => valuesHashes.contains(new ByteArrayWrapper(Algos.hash(elemValue.data)))} shouldBe true
  }
}
