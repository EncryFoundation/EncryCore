package encry.storage.levelDb.versionalLevelDB

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
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

  //Resolver tasks, contains versions in which keys should bew resolved
  // if resolverTasks empty - resolver set ACCESS_FLAG to 1
  var resolverTasks: ConcurrentLinkedQueue[LevelDBVersion] = new ConcurrentLinkedQueue[LevelDBVersion]()

  //Describe resolver status, if it is false - resolver "sleep", otherwise - busy
  var resolverStarted: AtomicBoolean = new AtomicBoolean(false)

  def versionsList: List[LevelDBVersion] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val result =
      splitValue2elems(KEY_SIZE, db.get(VERSIONS_LIST, readOptions)).map(elem => LevelDBVersion @@ elem)
    readOptions.snapshot().close()
    result
  }

  def currentVersion: LevelDBVersion = LevelDBVersion @@ db.get(CURRENT_VERSION_KEY)

  /**
    * @param elemKey
    * @return
    */
  def get(elemKey: VersionalLevelDbKey): Option[VersionalLevelDbValue] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val possibleToGet = if (db.get(ACCESS_FLAG, readOptions).last == (0: Byte)) {
      !versionsList.flatMap(version =>
        splitValue2elems(32, db.get(versionKey(version), readOptions)).map(key => new ByteArrayWrapper(key))
      ).contains(elemKey)
    } else true
    val possibleElem = if (possibleToGet) {
        if (db.get(ACCESSIBLE_KEY_PREFIX +: elemKey, readOptions) != null)
          Some(VersionalLevelDbValue @@ db.get(elemKey))
        else
          versionsList.find(version => db.get(accessableElementKeyForVersion(version, elemKey)) != null)
            .map(versionKey =>
              VersionalLevelDbValue @@  db.get(accessableElementKeyForVersion(versionKey, elemKey))
            )
    } else None
    readOptions.snapshot().close()
    possibleElem
  }

  /**
    * Insert new version to db.
    * @param newElem
    */
  def insert(newElem: LevelDbElem): Unit = {
    logger.info(s"Insert version: ${Algos.encode(newElem.version)}")
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    //Set current version to newElem.version
    batch.put(CURRENT_VERSION_KEY, newElem.version)
    //Insert new version to versions list
    batch.put(VERSIONS_LIST, newElem.version.untag(LevelDBVersion) ++ versionsList.flatMap(_.untag(LevelDBVersion)))
    //Ids of all elements, added by newElem
    batch.put(versionKey(newElem.version), newElem.elemsToInsert.map(_._1).flatMap(_.untag(LevelDBVersion)).toArray)
    //Ids of all elements, deleted by newElem
    batch.put(versionDeletionsKey(newElem.version), newElem.elemsToDelete.flatMap(_.untag(LevelDBVersion)).toArray)
    //Set resolve flag for version to false
    batch.put((SERVICE_PREFIX +: newElem.version) ++ RESOLVED_PREFIX, Array[Byte](0: Byte))
    newElem.elemsToInsert.foreach{
      case (elemKey, elemValue) =>
        /**
          * Put elem by key (ACCESSIBLE_KEY_PREFIX +: "version" ++ "elemKey")
          */
        if (db.get(accessableElementKeyForVersion(newElem.version, elemKey), readOptions) != null) {
          throw new Exception(s"Duplicate key: ${Algos.encode(elemKey)}")
        } else batch.put(accessableElementKeyForVersion(newElem.version, elemKey), elemValue)
    }
    //set access flag to false, means that resolver doesn't resolve element's access flags for this version
    batch.put(ACCESS_FLAG, Array[Byte](0))
    //write batch
    db.write(batch)
    batch.close()
    readOptions.snapshot().close()
//    resolverTasks.add(newElem.version)
//    if (!resolverStarted.get()) insertResolver()
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
          nextKey -> VersionalLevelDbValue @@ db.get(ACCESSIBLE_KEY_PREFIX +: nextKey, readOptions) :: acc
      }
    readOptions.snapshot().close()
    result
  }

  //Start resolver
  /**
    * Trying to resolve db keys(set correct access flags), based on current version.
    * if version elems keys is "resolved". Set key "SERVICE_PREFIX :: versrion ++ RESOLVED" prefix to true (1:Byte)
    * @return
    */
  def insertResolver(): Future[Unit] = Future {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    val versionToResolve: LevelDBVersion = resolverTasks.poll()
    val getDeletions = db.get(versionDeletionsKey(versionToResolve))
    //Check size of resolverTasks, is empty - set ACCESS_FLAG to true, disable resolver.
    //if nonEmpty, continue resolving
    if (resolverTasks.isEmpty) {
      db.put(ACCESS_FLAG, Array[Byte](1: Byte))
      resolverStarted.compareAndSet(true, false)
      db.write(batch)
      batch.close()
      readOptions.snapshot().close()
    } else {
      db.write(batch)
      batch.close()
      insertResolver()
      readOptions.snapshot().close()
    }
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
    logger.info(s"Current versions size: ${versions.length}")
    if (versions.length > settings.maxVersions) {
      logger.info("Max version qty. Delete last!")
      deleteVersion(versions.last)
    }
    db.write(batch)
    batch.close()
    readOptions.snapshot().close()
  }

  private def deleteVersion(versionToDelete: LevelDBVersion): Unit = {
    logger.info(s"Delete version: ${Algos.encode(versionToDelete)}")
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    logger.info("Max version qty. Delete last!")
    // get all acceptable keys
    val versions = versionsList
    val versionKeys = db.get(VERSION_PREFIX +: versionToDelete, readOptions)
    // remove all allias elements
    splitValue2elems(32, versionKeys).foreach{elemKey =>
      batch.delete((ACCESSIBLE_KEY_PREFIX +: versionToDelete) ++ elemKey)
    }
    // remove last version in versions list
    batch.delete(VERSION_PREFIX +: versionToDelete)
    // update versions list
    batch.put(VERSIONS_LIST, versions.filterNot(_ == versionToDelete)
      .foldLeft(Array.emptyByteArray){case (acc, key) => acc ++ key})
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
        buffer ::= VersionalLevelDbKey @@ nextKey
    }
    readOptions.snapshot().close()
    //logger.info(s"CurrentKeys: ${result.map(key => Algos.encode(key)).mkString(",")}")
    buffer
  }

  def inaccessibleKeys: List[VersionalLevelDbKey] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val result: List[VersionalLevelDbKey] =
      splitValue2elems(
        KEY_SIZE*2 + 1,
        db.get((VERSION_PREFIX +: DELETION_PREFIX) ++ currentVersion, readOptions)
      ).map(elem => VersionalLevelDbKey @@ elem)
    readOptions.snapshot().close()
    //logger.info(s"CurrentKeys: ${result.map(key => Algos.encode(key)).mkString(",")}")
    result
  }

  /**
    * Rollback to some point, just change current version to rollbackPoint, otherwise throw exeption
    * @param rollbackPoint
    */
  def rollbackTo(rollbackPoint: LevelDBVersion): Unit =
    if (versionsList.contains(rollbackPoint)) {
      val batch = db.createWriteBatch()
      batch.put(CURRENT_VERSION_KEY, rollbackPoint)
      db.write(batch)
      batch.close()
    } else throw new Exception(s"Impossible to rollback to ${Algos.encode(rollbackPoint)}")

  /**
    * Recover value by key or init with 'initValue'. Return resulted VersionalLevelDbValue
    */
  def recoverOrInitKey(key: VersionalLevelDbKey, initValue: VersionalLevelDbValue): VersionalLevelDbValue =
    if (db.get(key) == null) {
      val batch = db.createWriteBatch()
      batch.put(key, initValue)
      db.write(batch)
      logger.info(s"${Algos.encode(key)} is null. Set ${Algos.encode(key)} to ${Algos.encode(initValue)}")
      batch.close()
      initValue
    } else {
      logger.info(s"${Algos.encode(key)} exists!")
      VersionalLevelDbValue @@ db.get(key)
    }

  /**
    * Trying to recover previous values in db, otherwise reinit them by default values
    */
  def recoverOrInit(initValues: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map.empty): Unit = {
    val currentVersion = if (db.get(CURRENT_VERSION_KEY) == null) INIT_VERSION
      else db.get(CURRENT_VERSION_KEY)
    val initValuesWithVersion = initValues.map{case (key, value) => VersionalLevelDbKey @@ (currentVersion ++ key) -> value}
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

  object LevelDBVersion extends TaggedType[Array[Byte]]
  object VersionalLevelDbKey extends TaggedType[Array[Byte]]
  object VersionalLevelDbValue extends TaggedType[Array[Byte]]

  type LevelDBVersion = LevelDBVersion.Type
  type VersionalLevelDbKey = VersionalLevelDbKey.Type
  type VersionalLevelDbValue = VersionalLevelDbValue.Type

  val KEY_SIZE: Int = 32

  val SERVICE_PREFIX: Byte = 0
  val VERSION_PREFIX: Byte = 1
  val ACCESSIBLE_KEY_PREFIX: Byte = 2
  val INACCESSIBLE_KEY_PREFIX: Byte = 3

  val DELETION_PREFIX = Algos.hash("DELETION_SET")
  val RESOLVED_PREFIX = Algos.hash("RESOLVED")

  // Initial version id
  val INIT_VERSION: LevelDBVersion = LevelDBVersion @@ Array.fill(32)(0: Byte)

  //Key which set current version id
  val CURRENT_VERSION_KEY: VersionalLevelDbKey =
    VersionalLevelDbKey @@ (SERVICE_PREFIX +: Algos.hash("INIT_VERSION_KEY"))
  //Key, which set concatenation of fixed length keys, acceptable in current version
  val CURRENT_VERSION_LIST_KEY: VersionalLevelDbKey =
    VersionalLevelDbKey @@ (SERVICE_PREFIX +: Algos.hash("INIT_VERSION_LIST_KEY"))
  //Key, which set concatenation of fixed length keys, witch contains all acceptable versions
  val VERSIONS_LIST: VersionalLevelDbKey =
    VersionalLevelDbKey @@ (SERVICE_PREFIX +: Algos.hash("VERSIONS"))
  //Key, which describe that all access prefixes are resolved correctly
  val ACCESS_FLAG: VersionalLevelDbKey =
    VersionalLevelDbKey @@ (SERVICE_PREFIX +: Algos.hash("ACCESS_KEY"))

  /**
    * Initial keys
    */
  val INIT_MAP: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map(
    CURRENT_VERSION_KEY -> VersionalLevelDbValue @@ INIT_VERSION,
    CURRENT_VERSION_LIST_KEY -> VersionalLevelDbValue @@ Array.emptyByteArray,
    VERSIONS_LIST -> VersionalLevelDbValue @@ Array.emptyByteArray,
    ACCESS_FLAG -> VersionalLevelDbValue @@ Array(1: Byte)
  )

  def apply(levelDb: DB, settings: LevelDBSettings, initValues: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map.empty): VersionalLevelDB = {
    val db = VersionalLevelDB(levelDb, settings)
    db.recoverOrInit(initValues)
    db
  }
  
  def versionDeletionsKey(version: LevelDBVersion): Array[Byte] = (VERSION_PREFIX +: DELETION_PREFIX) ++ version
  
  def versionKey(version: LevelDBVersion): Array[Byte] = VERSION_PREFIX +: version

  def accessableElementKeyForVersion(version: LevelDBVersion, elemKey: VersionalLevelDbKey): Array[Byte] =
    (ACCESSIBLE_KEY_PREFIX +: version) ++ elemKey

  def splitValue2elems(elemSize: Int, value: Array[Byte]): List[Array[Byte]] = value.sliding(32, 32).toList
}
