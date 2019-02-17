package encry.storage.levelDb.versionalLevelDB

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean

import com.typesafe.scalalogging.StrictLogging
import encry.settings.LevelDBSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, _}
import io.iohk.iodb.ByteArrayWrapper
import org.apache.commons.lang.ArrayUtils
import org.encryfoundation.common.Algos
import org.iq80.leveldb.{DB, ReadOptions, WriteBatch}
import supertagged.TaggedType

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class VersionalLevelDB(db: DB, settings: LevelDBSettings) extends StrictLogging with AutoCloseable {

  //Insert resolver tasks, contains versions in which keys should be resolved during insertion
  // if resolverTasks empty - resolver set ACCESS_FLAG to 1
  val insertResolverTasks: ConcurrentLinkedQueue[LevelDBVersion] = new ConcurrentLinkedQueue[LevelDBVersion]()

  //Delete resolver tasks, contains versions in which keys should be resolved during deletion
  val deleteResolverTasks: ConcurrentLinkedQueue[LevelDBVersion] = new ConcurrentLinkedQueue[LevelDBVersion]()

  //Collect queue of versions, which should be deleted
  var toDeleteTask: ConcurrentLinkedQueue[LevelDBVersion] = new ConcurrentLinkedQueue[LevelDBVersion]()

  //Describe insert resolver status, if it is false - resolver "sleep", otherwise - busy
  var insertResolverStarted: AtomicBoolean = new AtomicBoolean(false)

  //Describe delete resolver status, if it is false - resolver "sleep", otherwise - busy
  var deleteResolverStarted: AtomicBoolean = new AtomicBoolean(false)

  def isDBresolved(readOptions: ReadOptions = new ReadOptions()): Boolean = {
    db.get(ACCESS_FLAG, readOptions).last == (1: Byte)
  }

  def versionsList(readOptions: ReadOptions = new ReadOptions()): List[LevelDBVersion] =
      splitValue2elems(KEY_SIZE, db.get(VERSIONS_LIST, readOptions)).map(elem => LevelDBVersion @@ elem)

  def currentVersion: LevelDBVersion = LevelDBVersion @@ db.get(CURRENT_VERSION_KEY)

  /**
    * @param elemKey
    * @return
    */
  //TODO: a lot None
  def get(elemKey: VersionalLevelDbKey): Option[VersionalLevelDbValue] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    logger.info(s"Trying to get: ${Algos.encode(elemKey)}")
    logger.info(s"Is db resolved: ${isDBresolved(readOptions)}")
    val possibleElem = if (!isDBresolved(readOptions)) {
      if (!versionsList(readOptions).flatMap(version =>
        splitValue2elems(32, db.get(versionKey(version), readOptions)).map(key => new ByteArrayWrapper(key))
      ).contains(elemKey)) {
        versionsList(readOptions).find(version => db.get(accessableElementKeyForVersion(version, elemKey)) != null)
          .map(versionKey =>
            VersionalLevelDbValue @@  db.get(accessableElementKeyForVersion(versionKey, elemKey))
          )
      } else None
    } else None
    if (db.get(userKey(elemKey)) != null &&
      isDBresolved(readOptions) &&
      possibleElem.isEmpty &&
      db.get(userKey(elemKey)).headOption.contains(ACCESSIBLE_KEY_PREFIX)) {
      logger.info(s"Trying to get access key ${Algos.encode(elemKey)}")
      val lastElemVersion: LevelDBVersion =
        LevelDBVersion @@ ArrayUtils.subarray(db.get(userKey(elemKey), readOptions), 1, 32)
      if (db.get(accessableElementKeyForVersion(lastElemVersion, elemKey)) != null)
        Some(VersionalLevelDbValue @@ db.get(accessableElementKeyForVersion(lastElemVersion, elemKey)))
      else None
    } else None
    readOptions.snapshot().close()
    possibleElem
  }

  /**
    * Insert new version to db.
    * @param newElem
    */
  //todo: refactor
  def insert(newElem: LevelDbElem): Unit = {
    logger.info(s"Insert version: ${Algos.encode(newElem.version)}")
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    //Set current version to newElem.version
    batch.put(CURRENT_VERSION_KEY, newElem.version)
    //Insert new version to versions list
    batch.put(VERSIONS_LIST, newElem.version.untag(LevelDBVersion) ++ versionsList(readOptions).flatMap(_.untag(LevelDBVersion)))
    //Ids of all elements, added by newElem
    batch.put(versionKey(newElem.version), newElem.elemsToInsert.map(_._1).flatMap(_.untag(LevelDBVersion)).toArray)
    //Ids of all elements, deleted by newElem
    batch.put(versionDeletionsKey(newElem.version), newElem.elemsToDelete.flatMap(_.untag(LevelDBVersion)).toArray)
    //Set resolve flag for version to false
    batch.put(versionResolvedKey(newElem.version), Array[Byte](0: Byte))
    newElem.elemsToInsert.foreach{
      case (elemKey, elemValue) =>
        /**
          * Put elem by key (ACCESSIBLE_KEY_PREFIX +: "version" ++ "elemKey")
          * First check contain db this elem or not. if no: insert elem, and insert init access map
          * if db contains elem, just insert elem
          */
        if (db.get(userKey(elemKey)) == null) {
          batch.put(userKey(elemKey), Array(ACCESSIBLE_KEY_PREFIX))
          logger.info(s"Insert key: ${Algos.encode(elemKey)}")
        }
        batch.put(accessableElementKeyForVersion(newElem.version, elemKey), elemValue)
    }
    //set access flag to false, means that resolver doesn't resolve element's access flags for this version
    batch.put(ACCESS_FLAG, Array[Byte](0))
    logger.info("Set ACCESS_FLAG to 0 in insert")
    //write batch
    db.write(batch)
    batch.close()
    readOptions.snapshot().close()
    insertResolverTasks.add(newElem.version)
    if (!insertResolverStarted.get()) insertResolver()
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
          nextKey -> VersionalLevelDbValue @@ db.get(accessableElementKey(nextKey), readOptions) :: acc
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
  //todo: refactor
  def insertResolver(): Future[Unit] = Future {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch: WriteBatch = db.createWriteBatch()
    val versionToResolve: LevelDBVersion = insertResolverTasks.poll()
    insertResolverStarted.getAndSet(true)
    logger.info(s"Init insert resolver for ${Algos.encode(versionToResolve)}")
    val deletions = splitValue2elems(KEY_SIZE,db.get(versionDeletionsKey(versionToResolve), readOptions))
    logger.info(s"Deletions of version: ${deletions.map(Algos.encode).mkString(",")}")
    val insertions = splitValue2elems(KEY_SIZE, db.get(versionKey(versionToResolve), readOptions))
    logger.info(s"Insertions of version: ${insertions.map(Algos.encode).mkString(",")}")
    insertions.foreach { elemKey =>
      val accessMap = db.get(userKey(VersionalLevelDbKey @@ elemKey), readOptions).tail
      logger.info(s"Previous access map in insert ${Algos.encode(elemKey)}: ${splitValue2elems(KEY_SIZE, accessMap).map(Algos.encode).mkString(",")}")
      batch.put(elemKey, (ACCESSIBLE_KEY_PREFIX +: versionToResolve) ++ accessMap)
    }
    deletions.foreach{ elemKey =>
      val accessMap = db.get(userKey(VersionalLevelDbKey @@ elemKey), readOptions).tail
      logger.info(s"Previous access map for deletions ${Algos.encode(elemKey)}: ${splitValue2elems(KEY_SIZE, accessMap).map(Algos.encode).mkString(",")}")
      logger.info(s"Set INACCESSIBLE_KEY_PREFIX to elem: ${Algos.encode(elemKey)}")
      batch.put(elemKey, INACCESSIBLE_KEY_PREFIX +: accessMap)
    }
    //Check size of resolverTasks, is empty - set ACCESS_FLAG to true, disable resolver.
    //if nonEmpty, continue resolving
    if (insertResolverTasks.isEmpty) {
      checkDbresolving(batch)
      insertResolverStarted.compareAndSet(true, false)
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
    * Db is resolved if resolvers "sleep"
    */
  def checkDbresolving(batch: WriteBatch): Unit =
    if (!insertResolverStarted.get() && !deleteResolverStarted.get()) {
      db.put(ACCESS_FLAG, Array[Byte](1: Byte))
    }

  /**
    * Remove unused keys
    * if version elems keys is "resolved". Set key "SERVICE_PREFIX :: versrion ++ RESOLVED" prefix to true (1:Byte)
    * @return
    */
  //todo: refactor
  def deleteResolver(): Future[Unit] = Future {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    val versionToResolve: LevelDBVersion = deleteResolverTasks.poll()
    deleteResolverStarted.getAndSet(true)
    logger.info(s"Init del resolver for ver: ${Algos.encode(versionToResolve)}")
    val wrappedVer: ByteArrayWrapper = new ByteArrayWrapper(versionToResolve)
    val deletionsByThisVersion = getInaccessiableKeysInVersion(versionToResolve, readOptions)
    deletionsByThisVersion.foreach{key =>
      val elemMap = db.get(userKey(key))
      val accessFlag = elemMap.head
      val elemVersions = splitValue2elems(32, elemMap.tail)
        .map(ver => new ByteArrayWrapper(ver))
        .filterNot(_ == wrappedVer)
      if (elemVersions.isEmpty) batch.delete(userKey(key))
      else batch.put(userKey(key), accessFlag +: elemVersions.foldLeft(Array.emptyByteArray){case (acc, ver) => acc ++ ver.data})
    }
    //Check size of resolverTasks, is empty - set ACCESS_FLAG to true, disable resolver.
    //if nonEmpty, continue resolving
    if (deleteResolverTasks.isEmpty) {
      checkDbresolving(batch)
      deleteResolverStarted.compareAndSet(true, false)
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
    * cleanIfPossible - if versionsList(readOptions).size > levelDB.maxVersions, delete last
    * also, delete elems without some ref
    */
  //TODO: Delete non alias elements without ref
  def clean(): Unit =  {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    val versions = versionsList().map(ver => new ByteArrayWrapper(ver))
    logger.info(s"Current versions size: ${versions.length}")
    if (versions.length > settings.maxVersions) {
      logger.info(s"Max version qty. Delete last!. Should del: ${versions.length - settings.maxVersions} versions")
      logger.info(versions.takeRight(versions.length - settings.maxVersions).map(v => Algos.encode(v.data)).mkString(","))
      val versionToDelete = versions.takeRight(versions.length - settings.maxVersions).reverse
      versionToDelete.foreach { v =>
        logger.info(s"Delete version: ${Algos.encode(v.data)}.Should del: ${versions.length - settings.maxVersions} versions")
        deleteResolverTasks.add(LevelDBVersion @@ v.data)
        // update versions list
      }
      batch.put(VERSIONS_LIST, versions.filterNot(versionToDelete.contains)
        .foldLeft(Array.emptyByteArray){case (acc, key) => acc ++ key.data})
    }
    db.write(batch)
    batch.close()
    readOptions.snapshot().close()
    deleteResolver()
  }

  /**
    * Get acceptable keys from current version
    * @return
    */
  def getCurrentElementsKeys: List[VersionalLevelDbKey] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val iter = db.iterator(readOptions)
    var buffer: List[VersionalLevelDbKey] = List.empty[VersionalLevelDbKey]
    while (iter.hasNext) {
      val nextKey = iter.peekNext().getKey
      if (nextKey.head == USER_KEY_PREFIX && !inaccessibleKeys(readOptions).contains(nextKey.tail))
        buffer ::= VersionalLevelDbKey @@ nextKey
    }
    readOptions.snapshot().close()
    //logger.info(s"CurrentKeys: ${result.map(key => Algos.encode(key)).mkString(",")}")
    buffer
  }

  def inaccessibleKeys(readOptions: ReadOptions = new ReadOptions()): List[VersionalLevelDbKey] = {
    val currentVersions = versionsList(readOptions)
    currentVersions.flatMap(version => getInaccessiableKeysInVersion(version, readOptions))
    //logger.info(s"CurrentKeys: ${result.map(key => Algos.encode(key)).mkString(",")}")
  }

  def getInaccessiableKeysInVersion(version: LevelDBVersion, readOptions: ReadOptions): List[VersionalLevelDbKey] = {
    splitValue2elems(
      KEY_SIZE,
      db.get(versionDeletionsKey(version), readOptions)
    ).map(elem => VersionalLevelDbKey @@ elem)
  }

  /**
    * Rollback to some point, just change current version to rollbackPoint, otherwise throw exeption
    * @param rollbackPoint
    */
  def rollbackTo(rollbackPoint: LevelDBVersion): Unit =
    if (versionsList().contains(rollbackPoint)) {
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

  def printKeysMap(): Unit = {
    getAll
  }

  // for debug only
  def print(): String = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val info = "VERSIONAL WRAPPER OF LEVEL DB Stat:\n " +
      s"Max versions qty: ${settings.maxVersions}\n " +
      s"Current version: ${Algos.encode(currentVersion)}\n " +
      s"Versions qty: ${versionsList(readOptions).length}\n " +
      s"Versions: [${versionsList(readOptions).map(Algos.encode).mkString(",")}]"
    info
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
    *   5 - User key
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
  val USER_KEY_PREFIX: Byte = 4

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
    versionResolvedKey(INIT_VERSION) ->  VersionalLevelDbValue @@ Array(1: Byte),
    ACCESS_FLAG -> VersionalLevelDbValue @@ Array(1: Byte)
  )

  def apply(levelDb: DB, settings: LevelDBSettings, initValues: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map.empty): VersionalLevelDB = {
    val db = VersionalLevelDB(levelDb, settings)
    db.recoverOrInit(initValues)
    db
  }
  
  def versionDeletionsKey(version: LevelDBVersion): VersionalLevelDbKey =
    VersionalLevelDbKey @@ ((VERSION_PREFIX +: DELETION_PREFIX) ++ version)

  def versionResolvedKey(version: LevelDBVersion): VersionalLevelDbKey =
    VersionalLevelDbKey @@ ((SERVICE_PREFIX +: RESOLVED_PREFIX) ++ version)
  
  def versionKey(version: LevelDBVersion): VersionalLevelDbKey =
    VersionalLevelDbKey @@ (VERSION_PREFIX +: version)

  def accessableElementKeyForVersion(version: LevelDBVersion, elemKey: VersionalLevelDbKey): VersionalLevelDbKey =
    VersionalLevelDbKey @@ ((ACCESSIBLE_KEY_PREFIX +: version) ++ elemKey)

  def accessableElementKey(elemKey: VersionalLevelDbKey): VersionalLevelDbKey =
    VersionalLevelDbKey @@ (ACCESSIBLE_KEY_PREFIX +: elemKey)

  def userKey(key: VersionalLevelDbKey): VersionalLevelDbKey =
    VersionalLevelDbKey @@ (USER_KEY_PREFIX +: key)

  def splitValue2elems(elemSize: Int, value: Array[Byte]): List[Array[Byte]] = value.sliding(32, 32).toList
}
