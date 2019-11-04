package encry.storage.levelDb.versionalLevelDB

import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, VersionalLevelDbValue}
import org.encryfoundation.common.utils.Algos

case class LevelDbDiff(version: LevelDBVersion,
                       elemsToInsert: List[(VersionalLevelDbKey, VersionalLevelDbValue)],
                       elemsToDelete: Seq[VersionalLevelDbKey] = Seq.empty) {
  override def toString: String = s"[LevelDB diff. Version: ${Algos.encode(version)}. keys to insert: " +
    s"${elemsToInsert.map(elem => Algos.encode(elem._1)).mkString(",")}, keys to delete: ${elemsToDelete.map(Algos.encode).mkString(",")}]"
}
