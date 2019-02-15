package encry.utils.levelDBUtils

import encry.storage.levelDb.versionalLevelDB.LevelDbElem
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, VersionalLevelDbValue}
import io.iohk.iodb.ByteArrayWrapper
import scorex.utils.Random

trait LevelDbUnitsGenerator {

  val defaultKeySize: Int = 32
  val defaultValueSize: Int = 256

  def generateRandomKey(keySize: Int = defaultKeySize): VersionalLevelDbKey =
    VersionalLevelDbKey @@ new ByteArrayWrapper(Random.randomBytes(keySize))

  def generateRandomValue(valueSize: Int = defaultValueSize): VersionalLevelDbValue =
    VersionalLevelDbValue @@ new ByteArrayWrapper(Random.randomBytes(valueSize))

  def genRandomInsertValue(keySize: Int = defaultKeySize,
                           valueSize: Int = defaultValueSize): (VersionalLevelDbKey, VersionalLevelDbValue) =
    (generateRandomKey(keySize), generateRandomValue(valueSize))

  def generateRandomLevelDbElemsWithoutDeletions(qty: Int, qtyOfElemsToInsert: Int): List[LevelDbElem] =
    (0 until qty).foldLeft(List.empty[LevelDbElem]) {
      case (acc, _) => LevelDbElem(
        LevelDBVersion @@ ByteArrayWrapper(Random.randomBytes()),
        List((0 until qtyOfElemsToInsert).map(_ => genRandomInsertValue()): _*),
        Seq.empty[VersionalLevelDbKey]
      ) :: acc
    }
}
