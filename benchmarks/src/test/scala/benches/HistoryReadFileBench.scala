package benches

import java.io.File
import java.util.concurrent.TimeUnit
import benches.HistoryReadFileBench.BenchStateHistory
import benches.Utils._
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
  def generateHistoryBench(benchStateHistory: BenchStateHistory, bh: Blackhole): Unit = {
    bh.consume {
      val history: EncryHistory = generateHistory(benchStateHistory.settings, benchStateHistory.tmpDir)
      history.closeStorage()
    }
  }
}

object HistoryReadFileBench {

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
      .addProfiler(classOf[GCProfiler])
      .warmupTime(TimeValue.milliseconds(500))
      .measurementTime(TimeValue.minutes(2))
      .build
    new Runner(opt).run
  }

  @State(Scope.Benchmark)
  class BenchStateHistory {

    val blocksNumber: Int = 15000
    val transactionsNumber: Int = 10
    val settings: EncryAppSettings = EncryAppSettings.read
    val tmpDir: File = getRandomTempDir

    @Setup
    def initializeHistory(): Unit = {
      val initialHistory: EncryHistory = generateHistory(settings, tmpDir)
      val resultedHistory: (EncryHistory, Option[Block]) = (0 until blocksNumber)
        .foldLeft(initialHistory, Option.empty[Block]) { case ((prevHistory, prevBlock), _) =>
          val block: Block =
            generateNextBlockValidForHistory(prevHistory, 0, prevBlock, transactionsNumber)
          (prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block),
            Some(block))
        }
      resultedHistory._1.closeStorage()
    }
  }
}