package benches

import java.io.File
import java.util.concurrent.TimeUnit

import benches.HistoryReadFileBench.BenchStateHistory
import benches.Utils._
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.history.Block
import encry.settings.EncryAppSettings
import encry.view.history.EncryHistory
import org.encryfoundation.common.Algos
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.OptionsBuilder

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
      .addProfiler(classOf[GCProfiler])
      .build
    new Runner(opt).run
  }

  @State(Scope.Benchmark)
  class BenchStateHistory {

    val blocksNumber: Int = 1000
    val settings: EncryAppSettings = EncryAppSettings.read
    val tmpDir: File = getRandomTempDir
    var history: Option[EncryHistory] = None

    @Setup
    def initializeHistory(): Unit = {
      val tmpHistory = generateHistory(settings, tmpDir)
      val genesisBlock: Block = generateGenesisBlock
      println(s"11111111 -> ${Algos.encode(genesisBlock.id)}")
      println(s"11111111 -> ${Algos.encode(genesisBlock.header.id)}")
      println(s"11111111 -> ${Algos.encode(genesisBlock.payload.id)}")
      val initHistory: EncryHistory = tmpHistory
        .append(genesisBlock.header).get._1
        .append(genesisBlock.payload).get._1
        .reportModifierIsValid(genesisBlock)

      val result: (Vector[Block], Block, EncryHistory) =
        (0 until blocksNumber).foldLeft(Vector.empty[Block], genesisBlock, initHistory) {
          case ((vector, block, historyL), _) =>
            val nextBlock: Block = generateNextBlock(historyL, block)
            println(s"${nextBlock.header.height}")
            val newHistory: EncryHistory = historyL
              .append(nextBlock.header).get._1
              .append(nextBlock.payload).get._1
              .reportModifierIsValid(nextBlock)
            (vector :+ nextBlock, nextBlock, newHistory)
        }
      history = Some(result._3)
      history.get.closeStorage()
    }
  }

}