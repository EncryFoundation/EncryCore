package benches

import java.io.File
import java.util.concurrent.TimeUnit

import benches.HistoryReadFileBench.BenchStateHistory
import benches.Utils._
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.history.Block
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}

class HistoryReadFileBench {

  @Benchmark
  def readOrGenerateHistoryBench(benchStateHistory: BenchStateHistory, bh: Blackhole): Unit = {
    bh.consume {
      val history: EncryHistory = generateHistory(benchStateHistory.settings, benchStateHistory.tmpDir)
      history.closeStorage()
    }
  }
}

object HistoryReadFileBench extends StrictLogging {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[HistoryReadFileBench].getSimpleName + ".*")
      .forks(1)
      .threads(1)
      .warmupIterations(10)
      .measurementIterations(10)
      .mode(Mode.AverageTime)
      .timeUnit(TimeUnit.SECONDS)
      .verbosity(VerboseMode.EXTRA)
      //.addProfiler(classOf[GCProfiler])
      .warmupTime(TimeValue.milliseconds(500))
      .measurementTime(TimeValue.milliseconds(500))
      .build
    new Runner(opt).run
  }

  @State(Scope.Benchmark)
  class BenchStateHistory {

    val blocksNumber: Int = 5000
    val settings: EncryAppSettings = EncryAppSettings.read
    val tmpDir: File = getRandomTempDir

    @Setup
    def initializeHistory(): Unit = {
      var tmpHistory: EncryHistory = generateHistory(settings, tmpDir)
      (0 until blocksNumber).foldLeft(tmpHistory) {
        case (prevHistory, t) =>
//          if (t % 3000 == 0) {
//            tmpHistory.closeStorage()
//            tmpHistory = generateHistory(settings, tmpDir)
//          }
          val block: Block = generateNextBlock(prevHistory)
          prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
      }
      tmpHistory.closeStorage()
    }
  }
}