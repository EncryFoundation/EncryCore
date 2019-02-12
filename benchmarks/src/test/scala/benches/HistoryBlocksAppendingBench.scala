package benches

import java.util.concurrent.TimeUnit
import benches.HistoryBlocksAppendingBench.BenchStateHistoryAppendingBlocks
import benches.Utils.{generateHistory, generateNextBlockValidForHistory, getRandomTempDir}
import encry.modifiers.history.Block
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}

class HistoryBlocksAppendingBench {

  @Benchmark
  def appendBlocksToHistoryBench(benchStateHistory: BenchStateHistoryAppendingBlocks, bh: Blackhole): Unit = {
    bh.consume {
      val history: EncryHistory = generateHistory(benchStateHistory.settings, getRandomTempDir)
      benchStateHistory.blocks.foldLeft(history) { case (historyL, block) =>
        historyL.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block)
      }
      history.closeStorage()
    }
  }
}

object HistoryBlocksAppendingBench {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[HistoryBlocksAppendingBench].getSimpleName + ".*")
      .forks(1)
      .threads(1)
      .warmupIterations(10)
      .measurementIterations(10)
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
  class BenchStateHistoryAppendingBlocks {

    val blocksNumber: Int = 15000
    val transactionsNumber: Int = 10
    val settings: EncryAppSettings = EncryAppSettings.read
    var blocks: Vector[Block] = Vector.empty[Block]

    @Setup
    def initializeHistory(): Unit = {
      val initialHistory: EncryHistory = generateHistory(settings, getRandomTempDir)
      val resultedHistory = (0 until blocksNumber)
        .foldLeft(initialHistory, Option.empty[Block], Vector.empty[Block]) {
          case ((prevHistory, prevBlock, vector), _) =>
            val block: Block =
              generateNextBlockValidForHistory(prevHistory, 0, prevBlock, transactionsNumber)
            (prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block),
              Some(block), vector :+ block)
        }
      resultedHistory._1.closeStorage()
      blocks = resultedHistory._3
    }
  }

}