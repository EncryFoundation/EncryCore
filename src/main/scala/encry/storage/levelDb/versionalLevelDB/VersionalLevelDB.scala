package encry.storage.levelDb.versionalLevelDB

import com.typesafe.scalalogging.StrictLogging
import encry.settings.LevelDBSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, _}
import io.iohk.iodb.ByteArrayWrapper
import org.apache.commons.lang.ArrayUtils
import org.encryfoundation.common.Algos
import org.iq80.leveldb.{DB, ReadOptions}
import supertagged.TaggedType

import scala.annotation.tailrec

case class VersionalLevelDB(db: DB, settings: LevelDBSettings) extends StrictLogging with AutoCloseable {

  def versionsList(readOptions: ReadOptions = new ReadOptions()): List[LevelDBVersion] =
    splitValue2elems(settings.versionKeySize, db.get(VERSIONS_LIST, readOptions)).map(elem => LevelDBVersion @@ elem)

  def currentVersion: LevelDBVersion = LevelDBVersion @@ db.get(CURRENT_VERSION_KEY)

  /**
    * @param elemKey
    * @return
    */
  //TODO: a lot None
  def get(elemKey: VersionalLevelDbKey): Option[VersionalLevelDbValue] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val possibleElemInResolved = if (db.get(userKey(elemKey)) != null &&
      db.get(userKey(elemKey)).headOption.contains(ACCESSIBLE_KEY_PREFIX)) {
      val lastElemVersion: LevelDBVersion =
        LevelDBVersion @@ ArrayUtils.subarray(db.get(userKey(elemKey), readOptions), 1, settings.versionKeySize + 1)
      if (db.get(accessableElementKeyForVersion(lastElemVersion, elemKey), readOptions) != null) {
        Some(VersionalLevelDbValue @@ db.get(accessableElementKeyForVersion(lastElemVersion, elemKey), readOptions))
      }
      else None
    } else None

    readOptions.snapshot().close()
    possibleElemInResolved
  }

  /**
    * Insert new version to db.
    *
    * @param newElem
    */
  //todo: refactor
  def insert(newElem: LevelDbElem): Unit = {
    assert(newElem.version.length == settings.versionKeySize,
      s"Version length is incorrect! Should be: ${settings.versionKeySize}, but get: ${newElem.version.length}")
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
    newElem.elemsToInsert.foreach {
      case (elemKey, elemValue) =>
        /**
          * Put elem by key (ACCESSIBLE_KEY_PREFIX +: "version" ++ "elemKey")
          * First check contain db this elem or not. if no: insert elem, and insert init access map
          * if db contains elem, just insert elem
          */
        if (db.get(userKey(elemKey)) == null) {
          batch.put(userKey(elemKey), ACCESSIBLE_KEY_PREFIX +: newElem.version)
        } else {
          val accessMap = db.get(userKey(elemKey), readOptions).tail
          batch.put(userKey(elemKey), (ACCESSIBLE_KEY_PREFIX +: newElem.version) ++ accessMap)
        }
        batch.put(accessableElementKeyForVersion(newElem.version, elemKey), elemValue)
    }
    newElem.elemsToDelete.foreach { elemKey =>
      val accessMap =
        if (db.get(userKey(elemKey), readOptions) != null) db.get(userKey(elemKey), readOptions).tail
        else Array.emptyByteArray
      batch.put(userKey(elemKey), INACCESSIBLE_KEY_PREFIX +: accessMap)
    }
    db.write(batch)
    batch.close()
    readOptions.snapshot().close()
    clean()
  }

  /**
    * Get all elems, stored in currentVersion.
    *
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

  /**
    * Remove all insertions of version after rollbackPoint
    *
    * @return
    */
  @tailrec
  private def rollbackResolver(versionsToResolve: List[LevelDBVersion]): Unit = {
    if (versionsToResolve.nonEmpty) {
      val versionToResolve = versionsToResolve.head
      val readOptions = new ReadOptions()
      readOptions.snapshot(db.getSnapshot)
      val writeBatch = db.createWriteBatch()
      val insertions = splitValue2elems(DEFAULT_USER_KEY_SIZE, db.get(versionKey(versionToResolve), readOptions))
      val deletions = splitValue2elems(DEFAULT_USER_KEY_SIZE, db.get(versionDeletionsKey(versionToResolve), readOptions))
      insertions.foreach { elemKey =>
        writeBatch.delete(accessableElementKeyForVersion(versionToResolve, VersionalLevelDbKey @@ elemKey))
        val accessMap = db.get(userKey(VersionalLevelDbKey @@ elemKey), readOptions)
        //only one version contains elem with this key, so remove it
        if (accessMap.length == settings.versionKeySize + 1)
          writeBatch.delete(userKey(VersionalLevelDbKey @@ elemKey))
        else {
          val versions =
            splitValue2elems(settings.versionKeySize, accessMap.tail).map(ver => new ByteArrayWrapper(ver))
              .dropWhile(_ != new ByteArrayWrapper(versionToResolve))
          val vertoput = versions.tail
            .foldLeft(Array.emptyByteArray) {
              case (acc, elem) => acc ++ elem.data
            }
          writeBatch.put(userKey(VersionalLevelDbKey @@ elemKey), ACCESSIBLE_KEY_PREFIX +: vertoput)
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
    }
  }

  /**
    * Remove unused keys
    * if version elems keys is "resolved". Set key "SERVICE_PREFIX :: versrion ++ RESOLVED" prefix to true (1:Byte)
    *
    * @return
    */
  //todo: refactor
  def deleteResolver(versionToResolve: LevelDBVersion): Unit = {
    val batch = db.createWriteBatch()
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val wrappedVer: ByteArrayWrapper = new ByteArrayWrapper(versionToResolve)
    val deletionsByThisVersion = getInaccessiableKeysInVersion(versionToResolve, readOptions)
    deletionsByThisVersion.foreach { key =>
      val elemMap = db.get(userKey(key), readOptions)
      val accessFlag = elemMap.head
      val elemVersions = splitValue2elems(settings.versionKeySize, elemMap.tail)
        .map(ver => new ByteArrayWrapper(ver))
        .filterNot(_ == wrappedVer)
      if (elemVersions.isEmpty) batch.delete(userKey(key))
      else batch.put(userKey(key), accessFlag +: elemVersions.foldLeft(Array.emptyByteArray) { case (acc, ver) => acc ++ ver.data })
    }
    val insertionsByThisVersion =
      splitValue2elems(DEFAULT_USER_KEY_SIZE, db.get(versionKey(versionToResolve))).map(VersionalLevelDbKey @@ _)
    insertionsByThisVersion.foreach{ elemKey =>
      val elemInfo = db.get(userKey(elemKey))
      val elemFlag = elemInfo.head
      val elemMap = elemInfo.tail
      val elemVersions = splitValue2elems(settings.versionKeySize, elemMap).map(ByteArrayWrapper.apply)
      elemVersions.dropWhile(_ != wrappedVer).tail.foreach{
        elemVerToDel => batch.delete(accessableElementKeyForVersion(LevelDBVersion @@ elemVerToDel.data, elemKey))
      }
      val newElemMap = elemVersions.takeWhile(_ != wrappedVer) :+ wrappedVer
      batch.put(userKey(elemKey), elemFlag +: newElemMap.foldLeft(Array.emptyByteArray) { case (acc, ver) => acc ++ ver.data })
    }
    db.write(batch)
    batch.close()
    readOptions.snapshot().close()
  }

  /**
    * cleanIfPossible - if versionsList(readOptions).size > levelDB.maxVersions, delete last
    * also, delete elems without some ref
    */
  //TODO: Delete non alias elements without ref
  def clean(): Unit = {
    val batch = db.createWriteBatch()
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val versions = versionsList(readOptions).map(ver => new ByteArrayWrapper(ver))
    if (versions.length > settings.maxVersions) {
      val versionToDelete = versions.last
      batch.put(VERSIONS_LIST, versions.init
        .foldLeft(Array.emptyByteArray) { case (acc, key) => acc ++ key.data })
      db.write(batch)
      batch.close()
      readOptions.snapshot().close()
      deleteResolver(LevelDBVersion @@ versionToDelete.data)
    }
  }

  /**
    * Get acceptable keys from current version
    *
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
    buffer
  }

  def inaccessibleKeys(readOptions: ReadOptions = new ReadOptions()): List[VersionalLevelDbKey] = {
    val currentVersions = versionsList(readOptions)
    currentVersions.flatMap(version => getInaccessiableKeysInVersion(version, readOptions))
  }

  def getInaccessiableKeysInVersion(version: LevelDBVersion, readOptions: ReadOptions): List[VersionalLevelDbKey] = {
    splitValue2elems(
      //size of elem key from settings
      32,
      db.get(versionDeletionsKey(version), readOptions)
    ).map(elem => VersionalLevelDbKey @@ elem)
  }

  /**
    * Rollback to some point, just change current version to rollbackPoint, otherwise throw exeption
    *
    * @param rollbackPoint
    */
  def rollbackTo(rollbackPoint: LevelDBVersion): Unit = {
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
      val verToDelete = allVerWrapped.takeWhile(_ != new ByteArrayWrapper(rollbackPoint))
      //Insert new version to versions list
      batch.put(VERSIONS_LIST, versionsBeforeRollback)
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
  def recoverOrInit(initValues: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map.empty,
                    keySize: Int = DEFAULT_VERSION_KEY_SIZE): Unit = {
    val currentVersion = if (db.get(CURRENT_VERSION_KEY) == null) INIT_VERSION(keySize)
    else db.get(CURRENT_VERSION_KEY)
    val initValuesWithVersion = initValues.map { case (key, value) => VersionalLevelDbKey @@ (currentVersion ++ key) -> value }
    (VersionalLevelDBCompanion.INIT_MAP(keySize) ++ initValuesWithVersion).foreach { case (key, value) => recoverOrInitKey(key, value) }
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
    * Prefix(Byte) + Key(Fixed-length byte array)
    * Prefix:
    * 0 - Level db service key (All VersionalLevelDBCompanion.INIT_MAP.keys)
    * 1 - Version key (all inserted elements)
    * 2 - Accessible key on current version
    * 3 - Inaccessible key on current version
    *    - User key
    */

  object LevelDBVersion extends TaggedType[Array[Byte]]

  object VersionalLevelDbKey extends TaggedType[Array[Byte]]

  object VersionalLevelDbValue extends TaggedType[Array[Byte]]

  type LevelDBVersion = LevelDBVersion.Type
  type VersionalLevelDbKey = VersionalLevelDbKey.Type
  type VersionalLevelDbValue = VersionalLevelDbValue.Type

  val DEFAULT_VERSION_KEY_SIZE: Int = 32
  val DEFAULT_USER_KEY_SIZE = 32

  val SERVICE_PREFIX: Byte = 0
  val VERSION_PREFIX: Byte = 1
  val ACCESSIBLE_KEY_PREFIX: Byte = 2
  val INACCESSIBLE_KEY_PREFIX: Byte = 3
  val USER_KEY_PREFIX: Byte = 4

  val DELETION_PREFIX = Algos.hash("DELETION_SET")

  // Initial version id
  def INIT_VERSION(KEY_SIZE: Int = DEFAULT_VERSION_KEY_SIZE): LevelDBVersion = LevelDBVersion @@ Array.fill(KEY_SIZE)(0: Byte)

  //Key which set current version id
  val CURRENT_VERSION_KEY: VersionalLevelDbKey =
    VersionalLevelDbKey @@ (SERVICE_PREFIX +: Algos.hash("INIT_VERSION_KEY"))
  //Key, which set concatenation of fixed length keys, acceptable in current version
  val CURRENT_VERSION_LIST_KEY: VersionalLevelDbKey =
    VersionalLevelDbKey @@ (SERVICE_PREFIX +: Algos.hash("INIT_VERSION_LIST_KEY"))
  //Key, which set concatenation of fixed length keys, witch contains all acceptable versions
  val VERSIONS_LIST: VersionalLevelDbKey =
    VersionalLevelDbKey @@ (SERVICE_PREFIX +: Algos.hash("VERSIONS"))

  /**
    * Initial keys
    */
  def INIT_MAP(keySize: Int = DEFAULT_VERSION_KEY_SIZE): Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map(
    CURRENT_VERSION_KEY -> VersionalLevelDbValue @@ INIT_VERSION(keySize),
    CURRENT_VERSION_LIST_KEY -> VersionalLevelDbValue @@ Array.emptyByteArray,
    VERSIONS_LIST -> VersionalLevelDbValue @@ INIT_VERSION(keySize),
    versionKey(INIT_VERSION(keySize)) -> VersionalLevelDbValue @@ Array.emptyByteArray,
    versionDeletionsKey(INIT_VERSION(keySize)) -> VersionalLevelDbValue @@ Array.emptyByteArray,
  )

  def apply(levelDb: DB,
            settings: LevelDBSettings,
            initValues: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map.empty,
            keySize: Int = DEFAULT_VERSION_KEY_SIZE): VersionalLevelDB = {
    val db = VersionalLevelDB(levelDb, settings)
    db.recoverOrInit(initValues, keySize)
    db
  }

  def versionDeletionsKey(version: LevelDBVersion): VersionalLevelDbKey =
    VersionalLevelDbKey @@ ((VERSION_PREFIX +: DELETION_PREFIX) ++ version)

  def versionKey(version: LevelDBVersion): VersionalLevelDbKey =
    VersionalLevelDbKey @@ (VERSION_PREFIX +: version)

  def accessableElementKeyForVersion(version: LevelDBVersion, elemKey: VersionalLevelDbKey): VersionalLevelDbKey =
    VersionalLevelDbKey @@ ((ACCESSIBLE_KEY_PREFIX +: version) ++ elemKey)

  def accessableElementKey(elemKey: VersionalLevelDbKey): VersionalLevelDbKey =
    VersionalLevelDbKey @@ (ACCESSIBLE_KEY_PREFIX +: elemKey)

  def userKey(key: VersionalLevelDbKey): VersionalLevelDbKey =
    VersionalLevelDbKey @@ (USER_KEY_PREFIX +: key)

  def splitValue2elems(elemSize: Int, value: Array[Byte]): List[Array[Byte]] = value.sliding(elemSize, elemSize).toList
}
