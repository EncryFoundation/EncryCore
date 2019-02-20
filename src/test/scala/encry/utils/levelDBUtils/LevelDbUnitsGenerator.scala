package encry.utils.levelDBUtils

import com.typesafe.scalalogging.StrictLogging
import encry.storage.levelDb.versionalLevelDB.LevelDbElem
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, VersionalLevelDbValue}
import io.iohk.iodb.ByteArrayWrapper
import scorex.utils.Random

import scala.util.{Random => ScalaRandom}

trait LevelDbUnitsGenerator extends StrictLogging {

  val defaultKeySize: Int = 32
  val defaultValueSize: Int = 256

  def generateRandomKey(keySize: Int = defaultKeySize): VersionalLevelDbKey =
    VersionalLevelDbKey @@ Random.randomBytes(keySize)

  def generateRandomValue(valueSize: Int = defaultValueSize): VersionalLevelDbValue =
    VersionalLevelDbValue @@ Random.randomBytes(valueSize)

  def genRandomInsertValue(keySize: Int = defaultKeySize,
                           valueSize: Int = defaultValueSize): (VersionalLevelDbKey, VersionalLevelDbValue) =
    (generateRandomKey(keySize), generateRandomValue(valueSize))

  def generateRandomLevelDbElemsWithoutDeletions(qty: Int, qtyOfElemsToInsert: Int): List[LevelDbElem] =
    (0 until qty).foldLeft(List.empty[LevelDbElem]) {
      case (acc, i) =>
        LevelDbElem(
          LevelDBVersion @@ Random.randomBytes(),
          List((0 until qtyOfElemsToInsert).map(_ => genRandomInsertValue()): _*)
        ) :: acc
    }

  /**
    * Generate list of levelDb elems, where each next elem delete random elem of previous insertions
    * @param qty
    * @param qtyOfElemsToInsert
    * @return
    */
  def generateRandomLevelDbElemsWithRandomDeletions(qty: Int, qtyOfElemsToInsert: Int): Seq[LevelDbElem] =
    (0 until qty).foldLeft(Seq.empty[LevelDbElem], Seq.empty[VersionalLevelDbKey]) {
      case ((acc, keys), _) =>
        val elemsToInsert = List((0 until qtyOfElemsToInsert).map(_ => genRandomInsertValue()): _*)
        val randomElemToDelete = keys(ScalaRandom.nextInt(keys.length))
        (acc :+ LevelDbElem(
          LevelDBVersion @@ Random.randomBytes(),
          elemsToInsert,
          Seq(randomElemToDelete)
        )) -> (keys.filter(_ != randomElemToDelete) ++ elemsToInsert.map(_._1))
    }._1

  /**
    * Generate list of levelDb elems, where each next elem delete all inserts of previous
    * @param qty
    * @param qtyOfElemsToInsert
    * @return
    */
  def generateRandomLevelDbElemsWithLinkedDeletions(qty: Int, qtyOfElemsToInsert: Int): Seq[LevelDbElem] =
    (0 until qty).foldLeft(Seq.empty[LevelDbElem]) {
      case (acc, _) =>
        acc :+ LevelDbElem(
          LevelDBVersion @@ Random.randomBytes(),
          List((0 until qtyOfElemsToInsert).map(_ => genRandomInsertValue()): _*),
          acc.lastOption.map(_.elemsToInsert.map(_._1)).getOrElse(Seq.empty[VersionalLevelDbKey])
        )
    }

  def generateRandomLevelDbElemsWithSameKeys(qty: Int, qtyOfElemsToInsert: Int): Seq[LevelDbElem] = {
    (0 until qty).foldLeft(Seq.empty[LevelDbElem]) {
      case (acc, _) =>
        acc :+ LevelDbElem(
          LevelDBVersion @@ Random.randomBytes(),
          acc.lastOption
            .map(_.elemsToInsert.map(elem => (elem._1, generateRandomValue())))
            .getOrElse(
              List((0 until qtyOfElemsToInsert).map(_ => genRandomInsertValue()): _*)
            ),
          Seq.empty[VersionalLevelDbKey]
        )
    }
  }
}
