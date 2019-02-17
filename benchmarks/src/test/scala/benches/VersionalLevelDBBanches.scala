package benches

import java.util.concurrent.TimeUnit
import benches.VersionalLevelDBBanches.VersionalLevelDBState
import encry.settings.LevelDBSettings
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import org.iq80.leveldb.Options
import org.openjdk.jmh.annotations.{Benchmark, Mode, Scope, State}
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}
import org.openjdk.jmh.runner.{Runner, RunnerException}

class VersionalLevelDBBanches {

  @Benchmark
  def appendBlocksToHistoryBench(benchStateHistory: VersionalLevelDBState, bh: Blackhole): Unit = {
    bh.consume {
      val tempDir = FileHelper.getRandomTempDir

      val levelDBInit = LevelDbFactory.factory.open(tempDir, new Options)

      val vldbInit = VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(100))

      benchStateHistory.elems1k.foreach(vldbInit.insert)

      vldbInit.close()
    }
  }
}

object VersionalLevelDBBanches {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[HistoryBenches].getSimpleName + ".*")
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

    val elems1k = Utils.generateRandomLevelDbElemsWithoutDeletions(1000, 100)
    val elems5k = Utils.generateRandomLevelDbElemsWithoutDeletions(1000, 100)
    val elems10k = Utils.generateRandomLevelDbElemsWithoutDeletions(1000, 100)
    val elems30k = Utils.generateRandomLevelDbElemsWithoutDeletions(1000, 100)
  }

}

