package benches

import java.util.concurrent.TimeUnit

import benches.HistoryBenches.HistoryBenchState
import encry.utils.TestEntityGenerator.coinbaseTransaction
import encry.utils.HistoryGenerator
import encry.view.history.History
import encryBenchmark.BenchSettings
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, ModifierId}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}
import scala.util.Random

class HistoryBenches {

  @Benchmark
  def appendBlocksToHistoryBench(benchStateHistory: HistoryBenchState, bh: Blackhole): Unit = {
    bh.consume {
      val history: History = HistoryGenerator.dummyHistory(benchStateHistory.settings)
      benchStateHistory.blocks.foldLeft(history) { case (historyL, block) =>
        historyL.append(block.header)
        historyL.append(block.payload)
        historyL.reportModifierIsValid(block)
      }
      history.closeStorage()
    }
  }

  @Benchmark
  def readHistoryFileBench(benchStateHistory: HistoryBenchState, bh: Blackhole): Unit = {
    bh.consume {
      val history: History = HistoryGenerator.dummyHistory(benchStateHistory.settings)
      history.closeStorage()
    }
  }
}

object HistoryBenches extends BenchSettings {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[HistoryBenches].getSimpleName + ".*")
      .forks(1)
      .threads(1)
      .warmupIterations(benchSettings.benchesSettings.warmUpIterations)
      .measurementIterations(benchSettings.benchesSettings.measurementIterations)
      .mode(Mode.AverageTime)
      .timeUnit(TimeUnit.SECONDS)
      .verbosity(VerboseMode.EXTRA)
      .addProfiler(classOf[GCProfiler])
      .warmupTime(TimeValue.milliseconds(benchSettings.benchesSettings.warmUpTime))
      .measurementTime(TimeValue.milliseconds(benchSettings.benchesSettings.measurementTime))
      .build
    new Runner(opt).run
  }

  @State(Scope.Benchmark)
  class HistoryBenchState extends encry.settings.Settings {

    val initialHistory: History = HistoryGenerator.dummyHistory(settings)

    val resultedHistory: (History, Option[Block], Vector[Block]) =
      (0 until benchSettings.historyBenchSettings.blocksNumber)
        .foldLeft(initialHistory, Option.empty[Block], Vector.empty[Block]) {
          case ((prevHistory, prevBlock, vector), _) =>
            val block: Block =
              generateNextBlockValidForHistory(prevHistory, 0, prevBlock,  Seq(coinbaseTransaction(0)), settings.constants.InitialDifficulty)
            prevHistory.append(block.header)
            prevHistory.append(block.payload)
            (prevHistory.reportModifierIsValid(block), Some(block), vector :+ block)
        }
    resultedHistory._1.closeStorage()

    val blocks: Vector[Block] = resultedHistory._3
  }

  def generateNextBlockValidForHistory(history: History,
                                       difficultyDiff: BigInt = 0,
                                       prevBlock: Option[Block],
                                       txs: Seq[Transaction],
                                       initialDifficulty: Difficulty): Block = {
    val previousHeaderId: ModifierId = prevBlock.map(_.id).getOrElse(Header.GenesisParentId)
    val requiredDifficulty: Difficulty = prevBlock.map(b =>
      history.requiredDifficultyAfter(b.header).getOrElse(Difficulty @@ BigInt(0)))
      .getOrElse(initialDifficulty)
    val header = Header(
      1.toByte,
      previousHeaderId,
      Payload.rootHash(txs.map(_.id)),
      Math.abs(Random.nextLong()),
      history.getBestHeaderHeight + 1,
      Random.nextLong(),
      Difficulty @@ (requiredDifficulty + difficultyDiff),
      EquihashSolution(Seq(1, 3))
    )
    Block(header, Payload(header.id, txs))
  }

}