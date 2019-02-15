package encry.storage.levelDb.versionalLevelDB

import com.typesafe.scalalogging.StrictLogging
import encry.settings.LevelDBSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, _}
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.Algos
import org.iq80.leveldb.{DB, ReadOptions}
import scorex.crypto.hash.Digest32
import supertagged.TaggedType
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class VersionalLevelDB(db: DB, settings: LevelDBSettings) extends StrictLogging with AutoCloseable {

  def versionsList: List[LevelDBVersion] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val result =
      splitValue2elems(KEY_SIZE, db.get(VERSIONS_LIST.data, readOptions)).map(elem => LevelDBVersion @@ new ByteArrayWrapper(elem))
    readOptions.snapshot().close()
    result
  }

  def currentVersion: LevelDBVersion = LevelDBVersion @@ new ByteArrayWrapper(db.get(CURRENT_VERSION_KEY.data))

  /**
    * First check ACCESS_FLAG:
    * if 1:
    *   Trying to get elem from db, first check if db contains elem by alias key ("version" ++ "elemKey"):
    *   if true - return elem
    *   else check if db contains elem by key "elemKey"
    *     if true - return elem
    *     else None
    * if 0:
    *   get inaccessibleKeys in all versions, and check if elem key contains in this seq
    *   if true:
    *     return None
    *   else: goto (if 1)
    * @param elemKey
    * @return
    */
  def get(elemKey: VersionalLevelDbKey): Option[VersionalLevelDbValue] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val possibleToGet = if (db.get(ACCESS_FLAG.data, readOptions).head == (0: Byte)) {
      !versionsList.flatMap(version =>
        splitValue2elems(32, db.get(version.data, readOptions)).map(key => new ByteArrayWrapper(key))
      ).contains(elemKey)
    } else true
    val possibleElem = if (possibleToGet) {
        if (db.get((ACCESSIBLE_KEY_PREFIX +: currentVersion.data) ++ elemKey.data, readOptions) != null)
          Some(VersionalLevelDbValue @@ new ByteArrayWrapper(db.get(currentVersion.data ++ elemKey.data)))
        else if (db.get(ACCESSIBLE_KEY_PREFIX +: elemKey.data, readOptions) != null)
          Some(VersionalLevelDbValue @@ new ByteArrayWrapper(db.get(elemKey.data)))
        else None
    } else None
    readOptions.snapshot().close()
    possibleElem
  }

  /**
    * Insert new version to db.
    * @param newElem
    */
  def insert(newElem: LevelDbElem): Unit = {
    logger.info(s"Insert version: ${Algos.encode(newElem.version.data)}")
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    batch.put(CURRENT_VERSION_KEY.data, newElem.version.data)
    batch.put(VERSION_PREFIX +: newElem.version.data, newElem.elemsToInsert.map(_._1).flatMap(_.data).toArray)
    batch.put((VERSION_PREFIX +: DELETION_PREFIX) ++ newElem.version.data, newElem.elemsToDelete.flatMap(_.data).toArray)
    newElem.elemsToInsert.foreach{
      case (elemKey, elemValue) =>
        /**
          * Check if db contains elem by key "elemkey":
          * if true:
          *   - Compare their hashes:
          *       if they are equal - do not put value, otherwise put by key (ACCESSIBLE_KEY_PREFIX +: "version" ++ "elemKey")
          * else:
          *   - put elem by key (ACCESSIBLE_KEY_PREFIX +: "elemKey")
          */
        if (db.get(ACCESSIBLE_KEY_PREFIX +: elemKey.data, readOptions) != null) {
          val elem = db.get(ACCESSIBLE_KEY_PREFIX +: elemKey.data, readOptions)
          if (new ByteArrayWrapper(Algos.hash(elem)) != new ByteArrayWrapper(Algos.hash(elemValue.data))) {
            batch.put((ACCESSIBLE_KEY_PREFIX +: newElem.version.data) ++ elemKey.data, elemValue.data)
          }
        } else batch.put(ACCESSIBLE_KEY_PREFIX +: elemKey.data, elemValue.data)
    }
    db.write(batch)
    batch.close()
    readOptions.snapshot().close()
    clean()
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
          nextKey -> VersionalLevelDbValue @@ new ByteArrayWrapper(db.get(ACCESSIBLE_KEY_PREFIX +: nextKey.data, readOptions)) :: acc
      }
    readOptions.snapshot().close()
    result
  }

  /**
    * cleanIfPossible - if versionsList.size > levelDB.maxVersions, delete last
    * also, delete elems without some ref
    */
  //TODO: Delete non alias elements without ref
  def clean(): Future[Unit] = Future {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    val versions = versionsList
    if (versions.length > settings.maxVersions) {
      logger.info("Max version qty. Delete last!")
      deleteVersion(versions.last)
    }
    db.write(batch)
    batch.close()
    readOptions.snapshot().close()
  }

  private def deleteVersion(versionToDelete: LevelDBVersion): Unit = {
    logger.info(s"Delete version: ${Algos.encode(versionToDelete.data)}")
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    logger.info("Max version qty. Delete last!")
    // get all acceptable keys
    val versions = versionsList
    val versionKeys = db.get(VERSION_PREFIX +: versionToDelete.data, readOptions)
    // remove all allias elements
    splitValue2elems(32, versionKeys).foreach{elemKey =>
      batch.delete((ACCESSIBLE_KEY_PREFIX +: versionToDelete.data) ++ elemKey)
    }
    // remove last version in versions list
    batch.delete(VERSION_PREFIX +: versionToDelete.data)
    // update versions list
    batch.put(VERSIONS_LIST.data, versions.filterNot(_ == versionToDelete)
      .foldLeft(Array.emptyByteArray){case (acc, key) => acc ++ key.data})
    db.write(batch)
    batch.close()
    readOptions.snapshot().close()
  }

  /**
    * Get acceptable keys from current version
    * @return
    */
  def getCurrentElementsKeys: List[VersionalLevelDbKey] = {
    val inacKeys = inaccessibleKeys
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val iter = db.iterator(readOptions)
    var buffer: List[VersionalLevelDbKey] = List.empty[VersionalLevelDbKey]
    while (iter.hasNext) {
      val nextKey = iter.peekNext().getKey
      if (nextKey.head == (2: Byte) && !inaccessibleKeys.contains(nextKey.tail))
        buffer ::= VersionalLevelDbKey @@ new ByteArrayWrapper(nextKey)
    }
    readOptions.snapshot().close()
    //logger.info(s"CurrentKeys: ${result.map(key => Algos.encode(key.data)).mkString(",")}")
    buffer
  }

  def inaccessibleKeys: List[VersionalLevelDbKey] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val result: List[VersionalLevelDbKey] =
      splitValue2elems(
        KEY_SIZE*2 + 1,
        db.get((VERSION_PREFIX +: DELETION_PREFIX) ++ currentVersion.data, readOptions)
      ).map(elem => VersionalLevelDbKey @@ new ByteArrayWrapper(elem))
    readOptions.snapshot().close()
    //logger.info(s"CurrentKeys: ${result.map(key => Algos.encode(key.data)).mkString(",")}")
    result
  }

  /**
    * Rollback to some point, just change current version to rollbackPoint, otherwise throw exeption
    * @param rollbackPoint
    */
  def rollbackTo(rollbackPoint: LevelDBVersion): Unit =
    if (versionsList.contains(rollbackPoint)) {
      val batch = db.createWriteBatch()
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

  /**
    * Info:
    * Key structure:
    *   Prefix(Byte) + Key(Fixed-length byte array)
    *   Prefix:
    *   0 - Level db service key (All VersionalLevelDBCompanion.INIT_MAP.keys)
    *   1 - Version key (all inserted elements)
    *   2 - Accessible key on current version
    *   3 - Inaccessible key on current version
    *   4 - Version key (all deleted elements)
    */

  object LevelDBVersion extends TaggedType[ByteArrayWrapper]
  object VersionalLevelDbKey extends TaggedType[ByteArrayWrapper]
  object VersionalLevelDbValue extends TaggedType[ByteArrayWrapper]

  type LevelDBVersion = LevelDBVersion.Type
  type VersionalLevelDbKey = VersionalLevelDbKey.Type
  type VersionalLevelDbValue = VersionalLevelDbValue.Type

  val KEY_SIZE: Int = 32

  val SERVICE_PREFIX: Byte = 0
  val VERSION_PREFIX: Byte = 1
  val ACCESSIBLE_KEY_PREFIX: Byte = 2
  val INACCESSIBLE_KEY_PREFIX: Byte = 3

  val DELETION_PREFIX = Algos.hash("DELETION_SET")

  // Initial version id
  val INIT_VERSION: LevelDBVersion = LevelDBVersion @@ new ByteArrayWrapper(Array.fill(32)(0: Byte))

  //Key which set current version id
  val CURRENT_VERSION_KEY: VersionalLevelDbKey =
    VersionalLevelDbKey @@ new ByteArrayWrapper(SERVICE_PREFIX +: Algos.hash("INIT_VERSION_KEY").untag(Digest32))
  //Key, which set concatenation of fixed length keys, acceptable in current version
  val CURRENT_VERSION_LIST_KEY: VersionalLevelDbKey =
    VersionalLevelDbKey @@ new ByteArrayWrapper(SERVICE_PREFIX +: Algos.hash("INIT_VERSION_LIST_KEY").untag(Digest32))
  //Key, which set concatenation of fixed length keys, witch contains all acceptable versions
  val VERSIONS_LIST: VersionalLevelDbKey =
    VersionalLevelDbKey @@ new ByteArrayWrapper(SERVICE_PREFIX +: Algos.hash("VERSIONS").untag(Digest32))
  //Key, which describe that all access prefixes are resolved correctly
  val ACCESS_FLAG: VersionalLevelDbKey =
    VersionalLevelDbKey @@ new ByteArrayWrapper(SERVICE_PREFIX +: Algos.hash("ACCESS_KEY").untag(Digest32))

  /**
    * Initial keys
    */
  val INIT_MAP: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map(
    CURRENT_VERSION_KEY -> VersionalLevelDbValue @@ INIT_VERSION.untag(LevelDBVersion),
    CURRENT_VERSION_LIST_KEY -> VersionalLevelDbValue @@ new ByteArrayWrapper(Array.emptyByteArray),
    VERSIONS_LIST -> VersionalLevelDbValue @@ new ByteArrayWrapper(Array.emptyByteArray),
    ACCESS_FLAG -> VersionalLevelDbValue @@ new ByteArrayWrapper(1: Byte)
  )

  def apply(levelDb: DB, settings: LevelDBSettings, initValues: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map.empty): VersionalLevelDB = {
    val db = VersionalLevelDB(levelDb, settings)
    db.recoverOrInit(initValues)
    db
  }

  def splitValue2elems(elemSize: Int, value: Array[Byte]): List[Array[Byte]] = value.sliding(32, 32).toList
}
