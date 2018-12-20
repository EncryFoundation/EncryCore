package encry.storage.levelDb

import com.typesafe.scalalogging.StrictLogging
import encry.storage.levelDb.VersionalLevelDBStorage.{Key, Value}
import encry.utils.CoreTaggedTypes.ModifierId
import org.iq80.leveldb.DB

case class VersionalLevelDBStorage(db: DB,
                                   var currentVersion: Array[Byte],
                                   var maxRollbachHeight: Int,
                                   var currentMaxHeight: Int,
                                   var acceptableKeys: Seq[ModifierId]) extends StrictLogging {

//  def get(key: Key): Option[Value] = {
//    db.getSnapshot
//  }

}

object VersionalLevelDBStorage {

  type Key = Array[Byte]
  type Value = Array[Byte]
}
