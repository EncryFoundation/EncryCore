package benches

import java.util.concurrent.TimeUnit

import benches.VersionalLevelDBBanches.VersionalLevelDBState
import encry.settings.LevelDBSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, VersionalLevelDbValue}
import encry.storage.levelDb.versionalLevelDB.{LevelDbDiff, LevelDbFactory, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import org.iq80.leveldb.Options
import org.openjdk.jmh.annotations.{Benchmark, Mode, Scope, State}
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}
import org.openjdk.jmh.runner.{Runner, RunnerException}
import scorex.utils.Random

class VersionalLevelDBBanches {

  @Benchmark
  def versionalLevelDbInsertion(benchStateHistory: VersionalLevelDBState, bh: Blackhole): Unit = {
    bh.consume {
      val tempDir = FileHelper.getRandomTempDir

      val levelDBInit = LevelDbFactory.factory.open(tempDir, new Options)

      val vldbInit = VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(100))

      benchStateHistory.elems10k.foreach(vldbInit.insert)

      vldbInit.close()
    }
  }
}

object VersionalLevelDBBanches {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[VersionalLevelDBBanches].getSimpleName + ".*")
      .forks(1)
      .threads(2)
      .warmupIterations(1)
      .measurementIterations(1)
      .mode(Mode.AverageTime)
      .timeUnit(TimeUnit.SECONDS)
      .verbosity(VerboseMode.EXTRA)
      .addProfiler(classOf[GCProfiler])
      .warmupTime(TimeValue.milliseconds(500))
      .measurementTime(TimeValue.minutes(5))
      .build
    new Runner(opt).run
  }

  @State(Scope.Benchmark)
  class VersionalLevelDBState {

    //val elems1k = generateRandomLevelDbElemsWithoutDeletions(1000, 100)
    //val elems5k = generateRandomLevelDbElemsWithoutDeletions(5000, 100)
    val elems10k = generateRandomLevelDbElemsWithoutDeletions(10000, 100)
    //val elems30k = generateRandomLevelDbElemsWithoutDeletions(30000, 100)
  }

  val defaultKeySize: Int = 32
  val defaultValueSize: Int = 256

  def generateRandomKey(keySize: Int = defaultKeySize): VersionalLevelDbKey =
    VersionalLevelDbKey @@ Random.randomBytes(keySize)

  def generateRandomValue(valueSize: Int = defaultValueSize): VersionalLevelDbValue =
    VersionalLevelDbValue @@ Random.randomBytes(valueSize)

  def genRandomInsertValue(keySize: Int = defaultKeySize,
                           valueSize: Int = defaultValueSize): (VersionalLevelDbKey, VersionalLevelDbValue) =
    (generateRandomKey(keySize), generateRandomValue(valueSize))

  def generateRandomLevelDbElemsWithoutDeletions(qty: Int, qtyOfElemsToInsert: Int): List[LevelDbDiff] =
    (0 until qty).foldLeft(List.empty[LevelDbDiff]) {
      case (acc, i) =>
        LevelDbDiff(
          LevelDBVersion @@ Random.randomBytes(),
          List((0 until qtyOfElemsToInsert).map(_ => genRandomInsertValue()): _*)
        ) :: acc
    }
}