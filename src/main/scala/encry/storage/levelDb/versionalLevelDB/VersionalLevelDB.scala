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

  //Describe rollback resolver status, if it is false - resolver "sleep", otherwise - busy
  var rollbackResolverStarted: AtomicBoolean = new AtomicBoolean(false)

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
    //logger.info("1")
    //logger.info(s"Trying to get: ${Algos.encode(elemKey)}")
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val possibleElem = if (!isDBresolved(readOptions)) {
      //logger.info("DB is not resolved")
      if (!versionsList(readOptions).flatMap(version =>
        splitValue2elems(32, db.get(versionKey(version), readOptions)).map(key => new ByteArrayWrapper(key))
      ).contains(elemKey)) {
        versionsList(readOptions).find(version => db.get(accessableElementKeyForVersion(version, elemKey)) != null)
          .map(versionKey =>
            VersionalLevelDbValue @@  db.get(accessableElementKeyForVersion(versionKey, elemKey))
          )
      } else None
    } else None
    //logger.info(s"(db.get(userKey(${Algos.encode(elemKey)})) != null: ${db.get(userKey(elemKey)) != null}")
    val possibleElemInResolved = if (db.get(userKey(elemKey)) != null &&
      isDBresolved(readOptions) &&
      possibleElem.isEmpty &&
      db.get(userKey(elemKey)).headOption.contains(ACCESSIBLE_KEY_PREFIX)) {
      //logger.info("DB is resolved")
      val lastElemVersion: LevelDBVersion =
        LevelDBVersion @@ ArrayUtils.subarray(db.get(userKey(elemKey), readOptions), 1, 33)
      //logger.info(s"Last version key: ${Algos.encode(lastElemVersion)}")
      //logger.info(s"Trying to get val by key: ${Algos.encode(accessableElementKeyForVersion(lastElemVersion, elemKey))}")
      if (db.get(accessableElementKeyForVersion(lastElemVersion, elemKey), readOptions) != null) {
        Some(VersionalLevelDbValue @@ db.get(accessableElementKeyForVersion(lastElemVersion, elemKey), readOptions))
      }
      else None
    } else None
    readOptions.snapshot().close()
    if (possibleElem.isEmpty) possibleElemInResolved else possibleElem
  }

  /**
    * Insert new version to db.
    * @param newElem
    */
  //todo: refactor
  def insert(newElem: LevelDbElem): Unit = {
    logger.info("2")
    logger.info(s"put version: ${Algos.encode(newElem.version)}")
    //remove this best thread pause
    do {logger.info(s"rollbackResolverStarted.get(): ${rollbackResolverStarted.get()}")} while (rollbackResolverStarted.get())
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
        }
        //logger.info(s"Put ${Algos.encode(accessableElementKeyForVersion(newElem.version, elemKey))}")
        batch.put(accessableElementKeyForVersion(newElem.version, elemKey), elemValue)
    }
    //set access flag to false, means that resolver doesn't resolve element's access flags for this version
    batch.put(ACCESS_FLAG, Array[Byte](0))
    //write batch
    db.write(batch)
    batch.close()
    readOptions.snapshot().close()
    insertResolverTasks.add(newElem.version)
    if (!insertResolverStarted.get()) initInsertResolver()
    clean()
  }

  /**
    * Get all elems, stored in currentVersion.
    * @return
    */
  def getAll: List[(VersionalLevelDbKey, VersionalLevelDbValue)] = {
    logger.info("3")
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
  def insertResolver(): Unit =  {
    logger.info("4")
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch: WriteBatch = db.createWriteBatch()
    val versionToResolve: LevelDBVersion = insertResolverTasks.poll()
    logger.info(s"Resolve ${Algos.encode(versionToResolve)}")
    val deletions = splitValue2elems(KEY_SIZE,db.get(versionDeletionsKey(versionToResolve), readOptions))
    val insertions = splitValue2elems(KEY_SIZE, db.get(versionKey(versionToResolve), readOptions))
    insertions.foreach { elemKey =>
      val accessMap = db.get(userKey(VersionalLevelDbKey @@ elemKey), readOptions).tail
//      logger.info(s"Get map by key: ${Algos.encode(userKey(VersionalLevelDbKey @@ elemKey))}. it contains: ${
//        splitValue2elems(32, accessMap).map(Algos.encode).mkString(",")
//      }")
      batch.put(userKey(VersionalLevelDbKey @@ elemKey), (ACCESSIBLE_KEY_PREFIX +: versionToResolve) ++ accessMap)
    }
    deletions.foreach{ elemKey =>
      val accessMap = db.get(userKey(VersionalLevelDbKey @@ elemKey), readOptions).tail
      batch.put(userKey(VersionalLevelDbKey @@ elemKey), INACCESSIBLE_KEY_PREFIX +: accessMap)
    }
    //Check size of resolverTasks, is empty - set ACCESS_FLAG to true, disable resolver.
    //if nonEmpty, continue resolving
    db.write(batch)
    batch.close()
    readOptions.snapshot().close()
    initInsertResolver()
  }

  def initInsertResolver(): Future[Unit] = Future {
    logger.info("5")
    if (!insertResolverTasks.isEmpty) {
      insertResolverStarted.compareAndSet(false, true)
      insertResolver()
    } else {
      insertResolverStarted.compareAndSet(true, false)
      val batch = db.createWriteBatch()
      checkDbresolving(batch)
      db.write(batch)
      batch.close()
      logger.info("insertResolverTasks is empty!")
    }
  }

  /**
    * Db is resolved if resolvers "sleep"
    */
  def checkDbresolving(batch: WriteBatch): Unit = {
    logger.info("6")
    if (!insertResolverStarted.get() && !deleteResolverStarted.get()) {
      db.put(ACCESS_FLAG, Array[Byte](1: Byte))
    }
  }

  /**
    * Remove all insertions of version after rollbackPoint
    * @return
    */
  def rollbackResolver(versionsToResolve: List[LevelDBVersion]): Future[Unit] = Future {
    logger.info("7")
    rollbackResolverStarted.set(true)
    if (versionsToResolve.nonEmpty) {
      val versionToResolve = versionsToResolve.head
      val readOptions = new ReadOptions()
      readOptions.snapshot(db.getSnapshot)
      val writeBatch = db.createWriteBatch()
      val insertions = splitValue2elems(32, db.get(versionKey(versionToResolve), readOptions))
      val deletions = splitValue2elems(32, db.get(versionDeletionsKey(versionToResolve), readOptions))
      insertions.foreach { elemKey =>
        writeBatch.delete(accessableElementKeyForVersion(versionToResolve, VersionalLevelDbKey @@ elemKey))
        val accessMap = db.get(userKey(VersionalLevelDbKey @@ elemKey), readOptions)
        //only one version contains elem with this key, so remove it
        if (accessMap.length == KEY_SIZE + 1)
          writeBatch.delete(userKey(VersionalLevelDbKey @@ elemKey))
        else {
          val versions =
            splitValue2elems(KEY_SIZE, accessMap.tail).map(ver => new ByteArrayWrapper(ver))
              .filter(_ != versionToResolve)
              .foldLeft(Array.emptyByteArray) {
                case (acc, elem) => acc ++ elem.data
              }
          writeBatch.put(userKey(VersionalLevelDbKey @@ elemKey), INACCESSIBLE_KEY_PREFIX +: versions)
        }
      }
      deletions.foreach { elemKey =>
        val accessMap = db.get(userKey(VersionalLevelDbKey @@ elemKey), readOptions)
        writeBatch.put(userKey(VersionalLevelDbKey @@ elemKey), ACCESSIBLE_KEY_PREFIX +: accessMap.tail)
      }
      db.write(writeBatch)
      writeBatch.close()
      readOptions.snapshot().close()
      rollbackResolver(versionsToResolve.tail)
    } else {
      rollbackResolverStarted.set(false)
    }
  }

  /**
    * Remove unused keys
    * if version elems keys is "resolved". Set key "SERVICE_PREFIX :: versrion ++ RESOLVED" prefix to true (1:Byte)
    * @return
    */
  //todo: refactor
  def deleteResolver(): Future[Unit] = Future {
    logger.info("8")
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    val versionToResolve: LevelDBVersion = deleteResolverTasks.poll()
    deleteResolverStarted.set(true)
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
      deleteResolverStarted.compareAndSet(true, false)
      db.write(batch)
      checkDbresolving(batch)
      batch.close()
      readOptions.snapshot().close()
    } else {
      db.write(batch)
      batch.close()
      deleteResolver()
      readOptions.snapshot().close()
    }
  }

  /**
    * cleanIfPossible - if versionsList(readOptions).size > levelDB.maxVersions, delete last
    * also, delete elems without some ref
    */
  //TODO: Delete non alias elements without ref
  def clean(): Unit =  {
    logger.info("9")
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    val versions = versionsList().map(ver => new ByteArrayWrapper(ver))
    if (versions.length > settings.maxVersions) {
      val versionToDelete = versions.takeRight(versions.length - settings.maxVersions).reverse
      versionToDelete.foreach { v =>
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
    logger.info("10")
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
    logger.info("11")
    val currentVersions = versionsList(readOptions)
    currentVersions.flatMap(version => getInaccessiableKeysInVersion(version, readOptions))
    //logger.info(s"CurrentKeys: ${result.map(key => Algos.encode(key)).mkString(",")}")
  }

  def getInaccessiableKeysInVersion(version: LevelDBVersion, readOptions: ReadOptions): List[VersionalLevelDbKey] = {
    logger.info("12")
    splitValue2elems(
      KEY_SIZE,
      db.get(versionDeletionsKey(version), readOptions)
    ).map(elem => VersionalLevelDbKey @@ elem)
  }

  /**
    * Rollback to some point, just change current version to rollbackPoint, otherwise throw exeption
    * @param rollbackPoint
    */
  def rollbackTo(rollbackPoint: LevelDBVersion): Unit = {
    logger.info("13")
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    if (versionsList(readOptions).map(ver => new ByteArrayWrapper(ver)).contains(new ByteArrayWrapper(rollbackPoint))) {
      val batch = db.createWriteBatch()
      batch.put(CURRENT_VERSION_KEY, rollbackPoint)
      val allVerWrapped = versionsList(readOptions).map(ver => new ByteArrayWrapper(ver))
      val versionsBeforeRollback = allVerWrapped
        .dropWhile(_ != new ByteArrayWrapper(rollbackPoint))
        .foldLeft(Array.emptyByteArray) {
          case (acc, ver) => acc ++ ver.data
        }
      val verToDelete = allVerWrapped.takeWhile(_ != versionsBeforeRollback)
      //Insert new version to versions list
      batch.put(VERSIONS_LIST, versionsBeforeRollback)
      batch.put(ACCESS_FLAG, Array(0: Byte))
      db.write(batch)
      rollbackResolver(verToDelete.map(LevelDBVersion @@ _.data))
      batch.close()
    } else throw new Exception(s"Impossible to rollback to ${Algos.encode(rollbackPoint)}")
    readOptions.snapshot().close()
  }

  /**
    * Recover value by key or init with 'initValue'. Return resulted VersionalLevelDbValue
    */
  def recoverOrInitKey(key: VersionalLevelDbKey, initValue: VersionalLevelDbValue): VersionalLevelDbValue = {
    logger.info("14")
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
  }

  /**
    * Trying to recover previous values in db, otherwise reinit them by default values
    */
  def recoverOrInit(initValues: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map.empty): Unit = {
    logger.info("15")
    val currentVersion = if (db.get(CURRENT_VERSION_KEY) == null) INIT_VERSION
      else db.get(CURRENT_VERSION_KEY)
    val initValuesWithVersion = initValues.map{case (key, value) => VersionalLevelDbKey @@ (currentVersion ++ key) -> value}
    (VersionalLevelDBCompanion.INIT_MAP ++ initValuesWithVersion).foreach { case (key, value) => recoverOrInitKey(key, value) }
  }

  def printKeysMap(): Unit = {
    logger.info("16")
    getAll
  }

  // for debug only
  def print(): String = {
    logger.info("17")
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
