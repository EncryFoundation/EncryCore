package encry.storage.levelDb.versionalLevelDB

import com.typesafe.scalalogging.StrictLogging
import encry.settings.LevelDBSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{VersionalLevelDbKey, VersionalLevelDbValue}
import encry.utils.FileHelper
import encry.utils.levelDBUtils.LevelDbUnitsGenerator
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.Algos
import org.iq80.leveldb.{Options, ReadOptions}
import org.scalatest.{Matchers, PropSpec}

import scala.concurrent.Await
import scala.util.Random

class VersionalLevelDBTest extends PropSpec with Matchers with LevelDbUnitsGenerator with StrictLogging{

  /**
    * Check correctness of data stored in level db, after insert(no deletions), close and init.
    * Check correctness of version and elems, stored in level db
    */
  property("LevelDB should recover to last version") {

    val maxVersions = Random.nextInt(300)

    val levelDbElemsQty = Random.nextInt(maxVersions)

    val dummyLevelDBSettings: LevelDBSettings = LevelDBSettings(maxVersions)

    val tempDir = FileHelper.getRandomTempDir

    val levelDBInit = LevelDbFactory.factory.open(tempDir, new Options)

    val vldbInit = VersionalLevelDBCompanion(levelDBInit, dummyLevelDBSettings)

    val levelDbElems = generateRandomLevelDbElemsWithoutDeletions(levelDbElemsQty, Random.nextInt(300))

    val keysToInsert: Seq[VersionalLevelDbKey] = levelDbElems.flatMap(_.elemsToInsert.map(_._1))

    val valuesHashes: List[ByteArrayWrapper] =
      levelDbElems.flatMap(_.elemsToInsert.map(elem => new ByteArrayWrapper(Algos.hash(elem._2))))

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
      .forall{case (_, elemValue) => valuesHashes.contains(new ByteArrayWrapper(Algos.hash(elemValue)))} shouldBe true
  }

  /**
    * Check correctness of data stored in level db, after insert(with linked deletions), close and init.
    * Check correctness of version and elems, stored in level db
    */
  property("LevelDB should recover to last version after linked deletions") {

    val maxVersions = Random.nextInt(1000)

    val levelDbElemsQty = Random.nextInt(maxVersions) + 1

    val dummyLevelDBSettings: LevelDBSettings = LevelDBSettings(maxVersions)

    val tempDir = FileHelper.getRandomTempDir

    val levelDBInit = LevelDbFactory.factory.open(tempDir, new Options)

    val vldbInit = VersionalLevelDBCompanion(levelDBInit, dummyLevelDBSettings)

    val levelDbElems = generateRandomLevelDbElemsWithLinkedDeletions(levelDbElemsQty, Random.nextInt(300))

    val correctKeys: Seq[VersionalLevelDbKey] = levelDbElems.last.elemsToInsert.map(_._1)

    val correctValues: Seq[ByteArrayWrapper] =
      levelDbElems.last.elemsToInsert.map{case (_, elem) => new ByteArrayWrapper(Algos.hash(elem))}

    val incorrectKeys: Seq[VersionalLevelDbKey] = levelDbElems.init.flatMap(_.elemsToInsert.map(_._1))

    levelDbElems.foreach(vldbInit.insert)

    vldbInit.close()

    val reopenedLevelDb = LevelDbFactory.factory.open(tempDir, new Options)

    val reopendVLDB = VersionalLevelDB(reopenedLevelDb, dummyLevelDBSettings)

    //Check correctness of version

    reopendVLDB.currentVersion shouldEqual levelDbElems.last.version

    //Check correctness of keys

    reopendVLDB.getAll.forall{case (elemKey, _) => correctKeys.contains(elemKey)} shouldBe true

    //Check correctness of values

    reopendVLDB.getAll
      .forall{case (_, elemValue) => correctValues.contains(new ByteArrayWrapper(Algos.hash(elemValue)))} shouldBe true

    //Check that impossible to get deleted keys

    val allElemsInDB = reopendVLDB.getAll.map(_._1)

    incorrectKeys.forall(key => !allElemsInDB.contains(key)) shouldBe true

    println(reopendVLDB.print())
  }

  property("Level db should always contains not more than maxVersionParam") {

    val maxVersions = Random.nextInt(1000)

    val levelDbElemsQty = Random.nextInt(1000) + maxVersions

    val dummyLevelDBSettings: LevelDBSettings = LevelDBSettings(maxVersions)

    val tempDir = FileHelper.getRandomTempDir

    val levelDBInit = LevelDbFactory.factory.open(tempDir, new Options)

    val vldbInit = VersionalLevelDBCompanion(levelDBInit, dummyLevelDBSettings)

    val levelDbElems = generateRandomLevelDbElemsWithoutDeletions(levelDbElemsQty, Random.nextInt(300))

    levelDbElems.foreach(vldbInit.insert)

    vldbInit.versionsList().length shouldEqual maxVersions
  }

  property("Level db should return previous value of elem after rollback. Add elem by same key, several times") {

    val maxVersions = 6

    val levelDbElemsQty = 10

    val dummyLevelDBSettings: LevelDBSettings = LevelDBSettings(maxVersions)

    val tempDir = FileHelper.getRandomTempDir

    val levelDBInit = LevelDbFactory.factory.open(tempDir, new Options)

    val vldbInit = VersionalLevelDBCompanion(levelDBInit, dummyLevelDBSettings)

    val levelDBElems = generateRandomLevelDbElemsWithSameKeys(levelDbElemsQty,1)

    levelDBElems.foreach(vldbInit.insert)

    vldbInit.rollbackTo(levelDBElems(6).version)

    logger.info("========================")
    logger.info(s"VERSIONS: ${levelDBElems.map(elem => Algos.encode(elem.version)).mkString(",")}")
    logger.info("========================")
    Algos.hash(vldbInit.get(levelDBElems(6).elemsToInsert.head._1).get) shouldEqual
      Algos.hash(levelDBElems(6).elemsToInsert.head._2)
  }

  property("deleted key from deleted version should not exist") {

    val maxVersions = 5

    val levelDbElemsQty = 3

    val dummyLevelDBSettings: LevelDBSettings = LevelDBSettings(maxVersions)

    val tempDir = FileHelper.getRandomTempDir

    val levelDBInit = LevelDbFactory.factory.open(tempDir, new Options)

    val vldbInit = VersionalLevelDBCompanion(levelDBInit, dummyLevelDBSettings)

    val levelDbElems = generateRandomLevelDbElemsWithLinkedDeletions(levelDbElemsQty, Random.nextInt(300))

    levelDbElems.foreach(vldbInit.insert)

    levelDbElems.last.elemsToInsert.forall{case (key, value) =>
      vldbInit.get(key).exists(dbValue => Algos.hash(dbValue) sameElements Algos.hash(value))
    } shouldBe true

    levelDbElems.last.elemsToDelete.forall{key =>
      vldbInit.get(key) == Option.empty[VersionalLevelDbValue]
    } shouldBe true
  }

  property("Check that after rollback, it is impossible to get last generated version") {

    val maxVersions = Random.nextInt(1000) + 15

    val levelDbElemsQty = Random.nextInt(maxVersions) + 10

    val rollbackPointIdx = Random.nextInt(levelDbElemsQty) + 5

    val dummyLevelDBSettings: LevelDBSettings = LevelDBSettings(maxVersions)

    val tempDir = FileHelper.getRandomTempDir

    val levelDBInit = LevelDbFactory.factory.open(tempDir, new Options)

    val vldbInit = VersionalLevelDBCompanion(levelDBInit, dummyLevelDBSettings)

    val levelDbElems = generateRandomLevelDbElemsWithLinkedDeletions(levelDbElemsQty, Random.nextInt(300))

    levelDbElems.foreach(vldbInit.insert)

    vldbInit.rollbackTo(levelDbElems(rollbackPointIdx).version)

    levelDbElems(rollbackPointIdx).elemsToInsert.forall{case (key, value) =>
      vldbInit.get(key).exists(dbValue => Algos.hash(dbValue) sameElements Algos.hash(value))
    } shouldBe true

    levelDbElems(rollbackPointIdx).elemsToDelete.forall{key =>
      vldbInit.get(key) == Option.empty[VersionalLevelDbValue]
    } shouldBe true

    levelDbElems(rollbackPointIdx + 1).elemsToInsert.forall{case (key, _) =>
      vldbInit.get(key) == Option.empty[VersionalLevelDbValue]
    } shouldBe true

    levelDbElems(rollbackPointIdx + 1).elemsToDelete.forall{key =>
      vldbInit.get(key).isDefined
    } shouldBe true

  }

  property("Size of user key shouldn't be greather then ((versions_list_size + 1)*version_key_size + 1)") {

    val maxVersions = Random.nextInt(1000) + 15

    val levelDbElemsQty = maxVersions + Random.nextInt(1000) + 10

    val dummyLevelDBSettings: LevelDBSettings = LevelDBSettings(maxVersions)

    val tempDir = FileHelper.getRandomTempDir

    val levelDBInit = LevelDbFactory.factory.open(tempDir, new Options)

    val vldbInit = VersionalLevelDBCompanion(levelDBInit, dummyLevelDBSettings)

    val levelDbElems: Seq[LevelDbElem] = generateRandomLevelDbElemsWithSameKeys(levelDbElemsQty, 2)

    levelDbElems.foreach(vldbInit.insert)

    vldbInit.db.get(VersionalLevelDBCompanion.userKey(levelDbElems.head.elemsToInsert.head._1)).length shouldEqual
      ((maxVersions + 1) * dummyLevelDBSettings.versionKeySize + 1)
  }
}
