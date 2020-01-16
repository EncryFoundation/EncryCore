package encry.storage.levelDb.versionalLevelDB

import com.typesafe.scalalogging.StrictLogging
import encry.settings.LevelDBSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, _}
import io.iohk.iodb.ByteArrayWrapper
import org.apache.commons.lang.ArrayUtils
import org.encryfoundation.common.utils.Algos
import org.iq80.leveldb.{DB, ReadOptions}
import supertagged.TaggedType
import java.util

case class VersionalLevelDB(db: DB, settings: LevelDBSettings) extends StrictLogging with AutoCloseable {

  var versionsList: List[LevelDBVersion] = versionsListInDB()

  def versionsListInDB(readOptions: ReadOptions = new ReadOptions()): List[LevelDBVersion] = {
    if (db.get(VERSIONS_LIST, readOptions) != null)
      splitValue2elems(settings.versionKeySize, db.get(VERSIONS_LIST, readOptions)).map(elem => LevelDBVersion @@ elem)
    else List.empty
  }

  def currentVersion: LevelDBVersion = LevelDBVersion @@ db.get(CURRENT_VERSION_KEY)

  def contains(elemKey: VersionalLevelDbKey): Boolean = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    try {
      val map: Array[Byte] = db.get(userKey(elemKey), readOptions)
      map != null && map.headOption.contains(ACCESSIBLE_KEY_PREFIX)
    } finally readOptions.snapshot().close()
  }

  /**
    * @param elemKey
    * @return
    */
  //TODO: a lot None
  def get(elemKey: VersionalLevelDbKey): Option[VersionalLevelDbValue] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    try {
      val map: Array[Byte] = db.get(userKey(elemKey), readOptions)
      val possibleElemInResolved = if (map != null &&
        map.headOption.contains(ACCESSIBLE_KEY_PREFIX)) {
        val lastElemVersion: LevelDBVersion =
          LevelDBVersion @@ ArrayUtils.subarray(map, 1, settings.versionKeySize + 1)
        val possibleElem: Array[Byte] =
          db.get(accessableElementKeyForVersion(lastElemVersion, elemKey), readOptions)
        if (possibleElem != null) {
          Some(VersionalLevelDbValue @@ possibleElem)
        }
        else None
      } else None
      possibleElemInResolved
    } finally readOptions.snapshot().close()
  }

  /**
    * Insert new version to db.
    *
    * @param newElem
    */
  //todo: refactor
  def insert(newElem: LevelDbDiff): Unit = {
    assert(newElem.version.length == settings.versionKeySize,
      s"Version length is incorrect! Should be: ${settings.versionKeySize}, but get: ${newElem.version.length}")
    assert(newElem.elemsToInsert.forall(_._1.length == settings.keySize),
      s"Key length is incorrect! Should be: ${settings.keySize}")
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val batch = db.createWriteBatch()
    try {
      //Set current version to newElem.version
      batch.put(CURRENT_VERSION_KEY, newElem.version)
      //Insert new version to versions list
      batch.put(VERSIONS_LIST, newElem.version.untag(LevelDBVersion) ++ versionsList.flatten)
      //Ids of all elements, added by newElem
      versionsList = newElem.version +: versionsList
      batch.put(versionKey(newElem.version), newElem.elemsToInsert.flatMap(_._1).toArray)
      //Ids of all elements, deleted by newElem
      batch.put(versionDeletionsKey(newElem.version), newElem.elemsToDelete.flatten.toArray)
      newElem.elemsToInsert.foreach {
        case (elemKey, elemValue) =>
          /**
            * Put elem by key (ACCESSIBLE_KEY_PREFIX +: "version" ++ "elemKey")
            * First check contain db this elem or not. if no: insert elem, and insert init access map
            * if db contains elem, just insert elem
            */
          val elemMap: Array[Byte] = db.get(userKey(elemKey), readOptions)
          if (elemMap == null) {
            batch.put(userKey(elemKey), ACCESSIBLE_KEY_PREFIX +: newElem.version)
          } else {
            val accessMap = util.Arrays.copyOfRange(elemMap, 1, elemMap.length)
            batch.put(userKey(elemKey), ArrayUtils.addAll(ACCESSIBLE_KEY_PREFIX +: newElem.version, accessMap))
          }
          batch.put(accessableElementKeyForVersion(newElem.version, elemKey), elemValue)
      }
      newElem.elemsToDelete.foreach { elemKey =>
        val possibleMap = db.get(userKey(elemKey), readOptions)
        if (possibleMap != null) {
          val accessMap = util.Arrays.copyOfRange(possibleMap, 1, possibleMap.length)
          batch.put(userKey(elemKey), INACCESSIBLE_KEY_PREFIX +: accessMap)
        } else
          logger.info(s"trying to delete empty key ${Algos.encode(elemKey)} in ver ${Algos.encode(newElem.version)}")
      }
      db.write(batch)
      clean()
    } finally {
      batch.close()
      readOptions.snapshot().close()
    }
  }

  /**
    * Get all elems, stored in currentVersion.
    * if maxQty = -1 return all data
    *
    * @return
    */
  def getAll(maxQty: Int = -1): List[(VersionalLevelDbKey, VersionalLevelDbValue)] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    try {
      getCurrentElementsKeys(maxQty)
        .foldLeft(List.empty[(VersionalLevelDbKey, VersionalLevelDbValue)]) {
          case (acc, nextKey) =>
            (nextKey -> get(nextKey).get) :: acc
        }
    } finally readOptions.snapshot().close()
  }

  /**
    * Remove all insertions of version after rollbackPoint
    *
    * @return
    */
  private def rollbackResolver(versionsToResolve: List[LevelDBVersion]): Unit = {
    if (versionsToResolve.nonEmpty) {
      val versionToResolve = versionsToResolve.head
      val readOptions = new ReadOptions()
      readOptions.snapshot(db.getSnapshot)
      val writeBatch = db.createWriteBatch()
      try {
        val insertions = splitValue2elems(DEFAULT_USER_KEY_SIZE, db.get(versionKey(versionToResolve), readOptions))
        val deletions = splitValue2elems(DEFAULT_USER_KEY_SIZE, db.get(versionDeletionsKey(versionToResolve), readOptions))
        insertions.foreach { elemKey =>
          writeBatch.delete(accessableElementKeyForVersion(versionToResolve, VersionalLevelDbKey @@ elemKey))
          val accessMap = db.get(userKey(VersionalLevelDbKey @@ elemKey), readOptions)
          //only one version contains elem with this key, so remove it
          if (accessMap.length == settings.versionKeySize + 1) {
            writeBatch.delete(userKey(VersionalLevelDbKey @@ elemKey))
            writeBatch.delete(accessableElementKeyForVersion(versionToResolve, VersionalLevelDbKey @@ elemKey))
          } else {
            val versions =
              splitValue2elems(settings.versionKeySize, accessMap.drop(1)).map(ver => new ByteArrayWrapper(ver))
                .dropWhile(_ != new ByteArrayWrapper(versionToResolve))
            val vertoput = versions.drop(1)
              .foldLeft(Array.emptyByteArray) {
                case (acc, elem) => acc ++ elem.data
              }
            writeBatch.put(userKey(VersionalLevelDbKey @@ elemKey), ACCESSIBLE_KEY_PREFIX +: vertoput)
          }
        }
        deletions.foreach { elemKey =>
          val accessMap = db.get(userKey(VersionalLevelDbKey @@ elemKey), readOptions)
          if (accessMap != null)
            writeBatch.put(userKey(VersionalLevelDbKey @@ elemKey), ACCESSIBLE_KEY_PREFIX +: accessMap.drop(1))
        }
        writeBatch.delete(versionKey(versionToResolve))
        writeBatch.delete(versionDeletionsKey(versionToResolve))
        db.write(writeBatch)
        if (versionsToResolve.nonEmpty) rollbackResolver(versionsToResolve.drop(1))
      } finally {
        writeBatch.close()
        readOptions.snapshot().close()
      }
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
    try {
      val wrappedVer: ByteArrayWrapper = ByteArrayWrapper(versionToResolve)
      val currentVersions: Set[ByteArrayWrapper] = versionsList.map(ByteArrayWrapper.apply).toSet
      val deletionsByThisVersion: Seq[VersionalLevelDbKey] =
        getInaccessiableKeysInVersion(versionToResolve, readOptions)
      deletionsByThisVersion.foreach { key =>
        val elemMap = db.get(userKey(key), readOptions)
        if (elemMap != null) {
          val wrappedVersions = splitValue2elems(settings.versionKeySize, elemMap.drop(1))
            .map(ver => new ByteArrayWrapper(ver))
          val elemVersions = wrappedVersions.filter(currentVersions.contains)
          val toDeleteElemsVersions = wrappedVersions.filterNot(currentVersions.contains)
          toDeleteElemsVersions.foreach(toDelWrapped =>
            batch.delete(accessableElementKeyForVersion(LevelDBVersion @@ toDelWrapped.data, key))
          )
          if (elemVersions.isEmpty) {
            batch.delete(userKey(key))
            batch.delete(accessableElementKeyForVersion(versionToResolve, key))
          }
          else batch.put(userKey(key), elemMap.head +: elemVersions.foldLeft(Array.emptyByteArray) { case (acc, ver) => acc ++ ver.data })
        }
      }
      val insertionsByThisVersion =
        splitValue2elems(DEFAULT_USER_KEY_SIZE, db.get(versionKey(versionToResolve), readOptions)).map(VersionalLevelDbKey @@ _)
      insertionsByThisVersion.foreach { elemKey =>
        val elemInfo = db.get(userKey(elemKey), readOptions)
        if (elemInfo == null) {
          logger.info(s"NULL at key: ${Algos.encode(elemKey)}. Deletion by ver: ${deletionsByThisVersion.map(Algos.encode).mkString(",")}")
        }
        val elemFlag = elemInfo.head
        val elemMap = util.Arrays.copyOfRange(elemInfo, 1, elemInfo.length + 1)
        //val elemMap = elemInfo.drop(1)
        if (elemMap.length > settings.versionKeySize) {
          val elemVersions = splitValue2elems(settings.versionKeySize, elemMap).map(ByteArrayWrapper.apply)
          elemVersions.dropWhile(_ != wrappedVer).drop(1).foreach {
            elemVerToDel => batch.delete(accessableElementKeyForVersion(LevelDBVersion @@ elemVerToDel.data, elemKey))
          }
          val newElemMap = elemVersions.takeWhile(_ != wrappedVer) :+ wrappedVer
          batch.put(userKey(elemKey), elemFlag +: newElemMap.foldLeft(Array.emptyByteArray) { case (acc, ver) => acc ++ ver.data })
        }
      }
      db.write(batch)
    } finally {
      batch.close()
      readOptions.snapshot().close()
    }
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
    try {
      val versions = versionsList.map(ver => new ByteArrayWrapper(ver))
      if (versions.length > settings.maxVersions) {
        val versionToDelete = versions.last
        batch.put(VERSIONS_LIST, versions.dropRight(1)
          .foldLeft(Array.emptyByteArray) { case (acc, key) => acc ++ key.data })
        versionsList = versionsList.dropRight(1)
        db.write(batch)
        deleteResolver(LevelDBVersion @@ versionToDelete.data)
      }
    } finally {
      batch.close()
      readOptions.snapshot().close()
    }
  }

  /**
    * Get acceptable keys from current version
    *
    * @return
    */
  def getCurrentElementsKeys(maxQty: Int = -1): List[VersionalLevelDbKey] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val iter = db.iterator(readOptions)
    try {
      iter.seekToFirst()
      var buffer: List[VersionalLevelDbKey] = List.empty[VersionalLevelDbKey]
      while (iter.hasNext && (maxQty == -1 || buffer.length < maxQty)) {
        val nextKey = iter.next().getKey
        if (nextKey.head == USER_KEY_PREFIX && db.get(nextKey, readOptions).headOption.contains(ACCESSIBLE_KEY_PREFIX)) {
          buffer ::= VersionalLevelDbKey @@ nextKey.drop(1)
        }
      }
      buffer
    } finally {
      iter.close()
      readOptions.snapshot().close()
    }
  }

  def inaccessibleKeys(readOptions: ReadOptions = new ReadOptions()): List[VersionalLevelDbKey] = {
    val currentVersions = versionsList
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
    * Rollback to some point, just change current version to rollbackPoint, otherwise throw exception
    *
    * @param rollbackPoint
    */
  def rollbackTo(rollbackPoint: LevelDBVersion): Unit = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    if (versionsList.map(ver => new ByteArrayWrapper(ver)).contains(new ByteArrayWrapper(rollbackPoint))) {
      val batch = db.createWriteBatch()
      try {
        batch.put(CURRENT_VERSION_KEY, rollbackPoint)
        val allVerWrapped = versionsList.map(ver => new ByteArrayWrapper(ver))
        val versionUnwrapped = allVerWrapped
          .dropWhile(_ != new ByteArrayWrapper(rollbackPoint))
        val versionsBeforeRollback = versionUnwrapped.foldLeft(Array.emptyByteArray) {
          case (acc, ver) => acc ++ ver.data
        }
        val verToDelete = allVerWrapped.takeWhile(_ != new ByteArrayWrapper(rollbackPoint))
        //Insert new version to versions list
        versionsList = versionUnwrapped.map(el => LevelDBVersion @@ el.data)
        batch.put(VERSIONS_LIST, versionsBeforeRollback)
        db.write(batch)
        rollbackResolver(verToDelete.map(LevelDBVersion @@ _.data))
      } finally {
        batch.close()
        readOptions.snapshot().close()
      }
    } else throw new Exception(s"Impossible to rollback to ${Algos.encode(rollbackPoint)}")
  }

  /**
    * Recover value by key or init with 'initValue'. Return resulted VersionalLevelDbValue
    */
  def recoverOrInitKey(key: VersionalLevelDbKey, initValue: VersionalLevelDbValue): VersionalLevelDbValue = {
    val batch = db.createWriteBatch()
    try {
      if (db.get(key) == null) {
        batch.put(key, initValue)
        db.write(batch)
        logger.debug(s"${Algos.encode(key)} is null. Set ${Algos.encode(key)} to ${Algos.encode(initValue)}")
        initValue
      } else {
        logger.debug(s"${Algos.encode(key)} exists!")
        VersionalLevelDbValue @@ db.get(key)
      }
    } finally batch.close()
  }

  /**
    * Trying to recover previous values in db, otherwise reinit them by default values
    */
  def recoverOrInit(initValues: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map.empty,
                    keySize: Int = DEFAULT_VERSION_KEY_SIZE): Unit = {
    val possibleCurVer = db.get(CURRENT_VERSION_KEY)
    val currentVersion = if (possibleCurVer == null) INIT_VERSION(keySize)
    else possibleCurVer
    val initValuesWithVersion = initValues.map { case (key, value) => VersionalLevelDbKey @@ (currentVersion ++ key) -> value }
    (VersionalLevelDBCompanion.INIT_MAP(keySize) ++ initValuesWithVersion).foreach { case (key, value) => recoverOrInitKey(key, value) }
  }

  override def toString: String = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val info = "VERSIONAL WRAPPER OF LEVEL DB Stat:\n " +
      s"Max versions qty: ${settings.maxVersions}\n " +
      s"Current version: ${Algos.encode(currentVersion)}\n " +
      s"Versions qty: ${versionsList.length}\n " +
      s"Versions: [${versionsList.map(Algos.encode).mkString(",")}]"
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

  val DELETION_PREFIX = Algos.hash("DELETION_SET")

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

  def splitValue2elems(elemSize: Int, value: Array[Byte] = Array.emptyByteArray): List[Array[Byte]] = {
    val resultArray = new Array[Array[Byte]](value.length / elemSize)
    (0 until (value.length / elemSize)).foreach {
      i =>
        val bufferArray = new Array[Byte](elemSize)
        System.arraycopy(value, i * elemSize, bufferArray, 0, elemSize)
        resultArray(i) = bufferArray
    }
    resultArray.toList
  }
}
