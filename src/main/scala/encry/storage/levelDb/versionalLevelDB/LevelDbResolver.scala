package encry.storage.levelDb.versionalLevelDB

import org.iq80.leveldb.DB
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Special auxiliary class, that contain functional to resolve references in VersionalLevelDB
  * Because of VersionalLevelDB contains values by version key in format:
  * ["Version key"] -> [VersionElemKey1.......VersionElemKeyN]
  * ["deletionsVersionKey"] -> [keyToDelete1.....keyToDelete2], where keyToDeleteN contains in previous version
  * i.e VersionElemKey1 can be:
  * 1. Reference to values, from previous version
  * 2. ElemKey
  * LevelDbResolver can resolve "Reference to values, from previous version", and paste elem of previous version instead of
  * reference
  * @param db
  */

