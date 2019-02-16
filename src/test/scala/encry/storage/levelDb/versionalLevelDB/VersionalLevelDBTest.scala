package encry.storage.levelDb.versionalLevelDB

import encry.settings.LevelDBSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{VersionalLevelDbKey, VersionalLevelDbValue}
import encry.utils.FileHelper
import encry.utils.levelDBUtils.LevelDbUnitsGenerator
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.Algos
import org.iq80.leveldb.Options
import org.scalatest.{Matchers, PropSpec}

import scala.concurrent.Await
import scala.util.Random

class VersionalLevelDBTest extends PropSpec with Matchers with LevelDbUnitsGenerator {

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

    println(s"Inac keys: ${reopendVLDB.inaccessibleKeys.map(elem => Algos.encode(elem)).mkString(",")}")

    incorrectKeys.forall(key => !allElemsInDB.contains(key)) shouldBe true
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

    //Thread.sleep(100000)

    do {
      println("1")
    } while (!vldbInit.resolverTasks.isEmpty)

    vldbInit.versionsList().length shouldEqual maxVersions

  }
}
