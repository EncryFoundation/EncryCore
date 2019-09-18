package encry.storage.levelDb.versionalLevelDB

import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, VersionalLevelDbValue}

case class LevelDbDiff(version: LevelDBVersion,
                       elemsToInsert: List[(VersionalLevelDbKey, VersionalLevelDbValue)],
                       elemsToDelete: Seq[VersionalLevelDbKey] = Seq.empty[VersionalLevelDbKey])
