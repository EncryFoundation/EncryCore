package encry.storage.levelDb.versionalLevelDB

import com.google.common.primitives.Longs
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.state.box.Box.Amount
import encry.modifiers.state.box.TokenIssuingBox.TokenId
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, _}
import encry.storage.levelDb.versionalLevelDB.WalletVersionalLevelDBCompanion.BALANCE_KEY
import io.iohk.iodb.ByteArrayWrapper
import org.encryfoundation.common.Algos
import org.iq80.leveldb.{DB, ReadOptions}
import scorex.crypto.hash.Digest32
import supertagged.TaggedType

case class VersionalLevelDB(db: DB) extends StrictLogging with AutoCloseable{

  def versionsList: List[LevelDBVersion] =
    splitValue2elems(KEY_SIZE, db.get(CURRENT_VERSION_LIST_KEY.data)).map(elem => LevelDBVersion @@ new ByteArrayWrapper(elem))

  def currentVersion: LevelDBVersion = LevelDBVersion @@ new ByteArrayWrapper(db.get(CURRENT_VERSION_KEY.data))

  def getTokenBalanceById(id: TokenId): Option[Amount] = getBalances
    .find(_._1 sameElements Algos.encode(id))
    .map(_._2)

  def getBalances: Map[String, Amount] =
    db.get(currentVersion.data ++ BALANCE_KEY.data).sliding(40, 40)
      .map(ch => Algos.encode(ch.take(32)) -> Longs.fromByteArray(ch.takeRight(8)))
      .toMap

  def get(key: VersionalLevelDbKey): Option[VersionalLevelDbValue] =
    if (db.get(currentVersion.data ++ key.data) != null)
      Some(VersionalLevelDbValue @@ new ByteArrayWrapper(db.get(currentVersion.data ++ key.data)))
    else None

  /**
    * Insert new version to db.
    * @param newElem
    */
  def insert(newElem: LevelDbElem): Unit = {
    val batch = db.createWriteBatch()
    batch.put(CURRENT_VERSION_KEY.data, newElem.version.data)
    val versionElemKeys = if (newElem.transferKeysFromPreviousVersion) {
      (newElem.elemsToInsert.map(_._1) ++ getCurrentElementsKeys.diff(newElem.elemsToDelete)).distinct
    } else newElem.elemsToInsert.map(_._1)
    batch.put(CURRENT_VERSION_LIST_KEY.data, versionElemKeys.foldLeft(Array.emptyByteArray) {
      case (acc, elem) => acc ++ elem.data
    })
    newElem.elemsToInsert.foreach{
      case (elemKey, elemValue) => batch.put(newElem.version.data ++ elemKey.data, elemValue.data)
    }
    db.delete(CURRENT_VERSION_KEY.data)
    db.delete(CURRENT_VERSION_LIST_KEY.data)
    db.write(batch)
    batch.close()
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
    * Get acceptable keys from current version
    * @return
    */
  def getCurrentElementsKeys: List[VersionalLevelDbKey] = {
    val readOptions = new ReadOptions()
    readOptions.snapshot(db.getSnapshot)
    val result: List[VersionalLevelDbKey] =
      splitValue2elems(KEY_SIZE*2, db.get(CURRENT_VERSION_LIST_KEY.data, readOptions)).map(elem => VersionalLevelDbKey @@ new ByteArrayWrapper(elem))
    readOptions.snapshot().close()
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

  val CURRENT_VERSION_KEY: VersionalLevelDbKey =
    VersionalLevelDbKey @@ new ByteArrayWrapper(Algos.hash("INIT_VERSION_KEY").untag(Digest32))
  val CURRENT_VERSION_LIST_KEY: VersionalLevelDbKey =
    VersionalLevelDbKey @@ new ByteArrayWrapper(Algos.hash("INIT_VERSION_LIST_KEY").untag(Digest32))

  val INIT_MAP: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map(
    CURRENT_VERSION_KEY -> VersionalLevelDbValue @@ INIT_VERSION.untag(LevelDBVersion),
    CURRENT_VERSION_LIST_KEY -> VersionalLevelDbValue @@ new ByteArrayWrapper(Array.emptyByteArray)
  )

  def apply(levelDb: DB, initValues: Map[VersionalLevelDbKey, VersionalLevelDbValue] = Map.empty): VersionalLevelDB = {
    val db = VersionalLevelDB(levelDb)
    db.recoverOrInit(initValues)
    db
  }

  def splitValue2elems(elemSize: Int, value: Array[Byte]): List[Array[Byte]] = value.sliding(32, 32).toList
}
