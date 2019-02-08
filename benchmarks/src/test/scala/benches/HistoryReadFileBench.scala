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
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue}

class HistoryReadFileBench {

  @Benchmark
  def readOrGenerateHistoryBench(benchStateHistory: BenchStateHistory, bh: Blackhole): Unit = {
    bh.consume {
      val history: EncryHistory = generateHistory(benchStateHistory.settings, benchStateHistory.tmpDir)
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
      .addProfiler(classOf[GCProfiler])
      .timeout(TimeValue.minutes(20))
      .warmupTime(TimeValue.minutes(20))
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
      val tmpHistory: EncryHistory = generateHistory(settings, tmpDir)
      (0 until blocksNumber).foldLeft(tmpHistory) {
        case (prevHistory, _) =>
          val block: Block = generateNextBlock(prevHistory)
          prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
      }
    }
  }

}