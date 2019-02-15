package encry.storage.levelDb.versionalLevelDB

import com.typesafe.scalalogging.StrictLogging
import encry.settings.LevelDBSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, _}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.Algos
import org.iq80.leveldb.{DB, ReadOptions}
import scorex.crypto.hash.Digest32
import supertagged.TaggedType
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class VersionalLevelDB(db: DB, settings: LevelDBSettings) extends StrictLogging with AutoCloseable {

  def versionsList: List[LevelDBVersion] =
    splitValue2elems(KEY_SIZE, db.get(VERSIONS_LIST.data)).map(elem => LevelDBVersion @@ new ByteArrayWrapper(elem))

  def currentVersion: LevelDBVersion = LevelDBVersion @@ new ByteArrayWrapper(db.get(CURRENT_VERSION_KEY.data))

  /**
    * Trying to get elem from db, first check if db contains elem by alias key ("version" ++ "elemKey"):
    * if true - return elem
    * else check if db contains elem by key "elemKey"
    *   if true - return elem
    *   else None
    * @param elemKey
    * @return
    */
  def get(elemKey: VersionalLevelDbKey): Option[VersionalLevelDbValue] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val possibleElem =
      if (db.get(currentVersion.data ++ elemKey.data, readOptions) != null)
        Some(VersionalLevelDbValue @@ new ByteArrayWrapper(db.get(currentVersion.data ++ elemKey.data)))
      else if (db.get(elemKey.data, readOptions) != null)
        Some(VersionalLevelDbValue @@ new ByteArrayWrapper(db.get(elemKey.data)))
      else None
    readOptions.snapshot().close()
    possibleElem
  }

  /**
    * Insert new version to db.
    * @param newElem
    */
  def insert(newElem: LevelDbElem): Unit = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    batch.delete(CURRENT_VERSION_KEY.data)
    batch.delete(CURRENT_VERSION_LIST_KEY.data)
    batch.put(CURRENT_VERSION_KEY.data, newElem.version.data)
    val versionElemKeys = if (newElem.transferKeysFromPreviousVersion) {
      (newElem.elemsToInsert.map(_._1) ++ getCurrentElementsKeys.diff(newElem.elemsToDelete)).distinct
    } else newElem.elemsToInsert.map(_._1)
    batch.put(CURRENT_VERSION_LIST_KEY.data, versionElemKeys.foldLeft(Array.emptyByteArray) {
      case (acc, elem) => acc ++ elem.data
    })
    newElem.elemsToInsert.foreach{
      case (elemKey, elemValue) =>
        /**
          * Check if db contains elem by key "elemkey":
          * if true:
          *   - Compare their hashes:
          *       if they are equal - do not put value, otherwise put by key "version" ++ "elemKey"
          * else:
          *   - put elem by key "elemKey"
          */
        if (db.get(elemKey.data, readOptions) != null) {
          val elem = db.get(elemKey.data, readOptions)
          if (new ByteArrayWrapper(Algos.hash(elem)) != new ByteArrayWrapper(Algos.hash(elemValue.data))) {
            batch.put(newElem.version.data ++ elemKey.data, elemValue.data)
          }
        } else batch.put(elemKey.data, elemValue.data)
    }
    db.write(batch)
    batch.close()
    readOptions.snapshot().close()
    cleanIfPossible()
  }

  /**
    * Get all elems, stored in currentVersion.
    * @return
    */
  def getAll: List[(VersionalLevelDbKey, VersionalLevelDbValue)] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val result = getCurrentElementsKeys
      .foldLeft(List.empty[(VersionalLevelDbKey, VersionalLevelDbValue)]) {
        case (acc, nextKey) =>
          nextKey -> VersionalLevelDbValue @@ new ByteArrayWrapper(db.get(nextKey.data, readOptions)) :: acc
      }
    readOptions.snapshot().close()
    result
  }

  /**
    * cleanIfPossible - in case if versionsList.size > levelDB.maxVersions, delete last
    * also, delete elems without some ref
    */
  def cleanIfPossible(): Future[Unit] = Future {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    val versions = versionsList
    if (versions.length > settings.maxVersions) {
      logger.info("Max version qty. Delete last!")
      // get all acceptable keys
      val versionKeys = db.get(versions.last.data, readOptions)
      // remove all elements
      splitValue2elems(32, versionKeys).foreach{elemKey =>
        batch.delete(versions.last.data ++ elemKey)
      }
      // remove version
      batch.delete(versions.last.data)
      // update versions list
      batch.delete(VERSIONS_LIST.data)
      batch.put(VERSIONS_LIST.data, versions.init.foldLeft(Array.emptyByteArray){case (acc, key) => acc ++ key.data})
    }
    db.write(batch)
    batch.close()
    readOptions.snapshot().close()
  }

  /**
    * Get acceptable keys from current version
    * @return
    */
  def getCurrentElementsKeys: List[VersionalLevelDbKey] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val result: List[VersionalLevelDbKey] =
      splitValue2elems(KEY_SIZE*2, db.get(CURRENT_VERSION_LIST_KEY.data, readOptions)).map(elem => VersionalLevelDbKey @@ new ByteArrayWrapper(elem))
    readOptions.snapshot().close()
    logger.info(s"CurrentKeys: ${result.map(key => Algos.encode(key.data)).mkString(",")}")
    result
  }

  /**
    * Rollback to some point, just change current version to rollbackPoint, otherwise throw exeption
    * @param rollbackPoint
    */
  def rollbackTo(rollbackPoint: LevelDBVersion): Unit =
    if (versionsList.contains(rollbackPoint)) {
      val batch = db.createWriteBatch()
      batch.delete(CURRENT_VERSION_KEY.data)
      batch.put(CURRENT_VERSION_KEY.data, rollbackPoint.data)
      db.write(batch)
      batch.close()
    } else throw new Exception(s"Impossible to rollback to ${Algos.encode(rollbackPoint.data)}")

  /**
    * Recover value by key or init with 'initValue'. Return resulted VersionalLevelDbValue
    */
  def recoverOrInitKey(key: VersionalLevelDbKey, initValue: VersionalLevelDbValue): VersionalLevelDbValue =
    if (db.get(key.data) == null) {
      val batch = db.createWriteBatch()
      batch.put(key.data, initValue.data)
      db.write(batch)
      logger.info(s"${Algos.encode(key.data)} is null. Set ${Algos.encode(key.data)} to ${Algos.encode(initValue.data)}")
      batch.close()
      initValue
    } else {
      logger.info(s"${Algos.encode(key.data)} exists!")
      VersionalLevelDbValue @@ new ByteArrayWrapper(db.get(key.data))
    }

  /**
    * Trying to recover previous values in db, otherwise reinit them by default values
    */
  def recoverOrInit(initValues: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map.empty): Unit = {
    val currentVersion = if (db.get(CURRENT_VERSION_KEY.data) == null) INIT_VERSION.data
      else db.get(CURRENT_VERSION_KEY.data)
    val initValuesWithVersion = initValues.map{case (key, value) => VersionalLevelDbKey @@ new ByteArrayWrapper(currentVersion ++ key.data) -> value}
    (VersionalLevelDBCompanion.INIT_MAP ++ initValuesWithVersion).foreach { case (key, value) => recoverOrInitKey(key, value) }
  }

  override def close(): Unit = db.close()
}

object VersionalLevelDBCompanion {

  object LevelDBVersion extends TaggedType[ByteArrayWrapper]
  object VersionalLevelDbKey extends TaggedType[ByteArrayWrapper]
  object VersionalLevelDbValue extends TaggedType[ByteArrayWrapper]

  type LevelDBVersion = LevelDBVersion.Type
  type VersionalLevelDbKey = VersionalLevelDbKey.Type
  type VersionalLevelDbValue = VersionalLevelDbValue.Type

  val KEY_SIZE: Int = 32

  // Initial version id
  val INIT_VERSION: LevelDBVersion = LevelDBVersion @@ new ByteArrayWrapper(Array.fill(32)(0: Byte))

  //Key which set current version id
  val CURRENT_VERSION_KEY: VersionalLevelDbKey =
    VersionalLevelDbKey @@ new ByteArrayWrapper(Algos.hash("INIT_VERSION_KEY").untag(Digest32))
  //Key, which set concatenation of fixed length keys, acceptable in current version
  val CURRENT_VERSION_LIST_KEY: VersionalLevelDbKey =
    VersionalLevelDbKey @@ new ByteArrayWrapper(Algos.hash("INIT_VERSION_LIST_KEY").untag(Digest32))
  //Key, which set concatenation of fixed length keys, witch contains all acceptable versions
  val VERSIONS_LIST: VersionalLevelDbKey =
    VersionalLevelDbKey @@ new ByteArrayWrapper(Algos.hash("VERSIONS").untag(Digest32))

  /**
    * Initial keys
    */
  val INIT_MAP: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map(
    CURRENT_VERSION_KEY -> VersionalLevelDbValue @@ INIT_VERSION.untag(LevelDBVersion),
    CURRENT_VERSION_LIST_KEY -> VersionalLevelDbValue @@ new ByteArrayWrapper(Array.emptyByteArray),
    VERSIONS_LIST -> VersionalLevelDbValue @@ new ByteArrayWrapper(Array.emptyByteArray)
  )

  def apply(levelDb: DB, settings: LevelDBSettings, initValues: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map.empty): VersionalLevelDB = {
    val db = VersionalLevelDB(levelDb, settings)
    db.recoverOrInit(initValues)
    db
  }

  def splitValue2elems(elemSize: Int, value: Array[Byte]): List[Array[Byte]] = value.sliding(32, 32).toList
}
