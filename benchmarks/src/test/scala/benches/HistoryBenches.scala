//package benches
//
//import java.io.File
//import java.util.concurrent.TimeUnit
//import benches.HistoryBenches.HistoryBenchState
//import benches.Utils.{generateHistory, generateNextBlockValidForHistory, getRandomTempDir}
//import encry.settings.EncryAppSettings
//import encry.view.history.History
//import encryBenchmark.Settings
//import org.encryfoundation.common.modifiers.history.Block
//import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
//import org.openjdk.jmh.annotations._
//import org.openjdk.jmh.infra.Blackhole
//import org.openjdk.jmh.profile.GCProfiler
//import org.openjdk.jmh.runner.{Runner, RunnerException}
//import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}
//
//class HistoryBenches {
//
//  @Benchmark
//  def appendBlocksToHistoryBench(benchStateHistory: HistoryBenchState, bh: Blackhole): Unit = {
//    bh.consume {
//      val history: History = generateHistory(benchStateHistory.settings, getRandomTempDir)
////      benchStateHistory.blocks.foldLeft(history) { case (historyL, block) =>
////        historyL.append(block.header).right.get._1.append(block.payload).right.get._1.reportModifierIsValid(block)
////      }
//      history.closeStorage()
//    }
//  }
//
//  @Benchmark
//  def readHistoryFileBench(benchStateHistory: HistoryBenchState, bh: Blackhole): Unit = {
//    bh.consume {
//      val history: History = generateHistory(benchStateHistory.settings, benchStateHistory.tmpDir)
//      history.closeStorage()
//    }
//  }
//}
//
//object HistoryBenches {
//
//  val benchSettings: Settings = Settings.read
//
//  @throws[RunnerException]
//  def main(args: Array[String]): Unit = {
//    val opt = new OptionsBuilder()
//      .include(".*" + classOf[HistoryBenches].getSimpleName + ".*")
//      .forks(1)
//      .threads(1)
//      .warmupIterations(benchSettings.benchesSettings.warmUpIterations)
//      .measurementIterations(benchSettings.benchesSettings.measurementIterations)
//      .mode(Mode.AverageTime)
//      .timeUnit(TimeUnit.SECONDS)
//      .verbosity(VerboseMode.EXTRA)
//      .addProfiler(classOf[GCProfiler])
//      .warmupTime(TimeValue.milliseconds(benchSettings.benchesSettings.warmUpTime))
//      .measurementTime(TimeValue.milliseconds(benchSettings.benchesSettings.measurementTime))
//      .build
//    new Runner(opt).run
//  }
//
//  @State(Scope.Benchmark)
//  class HistoryBenchState {
//
//    val settings: EncryAppSettings = EncryAppSettings.read
//    val tmpDir: File = getRandomTempDir
//    val initialHistory: History = generateHistory(settings, tmpDir)
//
//    val resultedHistory: (History, Option[Block], Vector[Block]) =
//      (0 until benchSettings.historyBenchSettings.blocksNumber)
//        .foldLeft(initialHistory, Option.empty[Block], Vector.empty[Block]) {
//          case ((prevHistory, prevBlock, vector), _) =>
//            val block: Block =
//              generateNextBlockValidForHistory(
//                prevHistory, 0, prevBlock,  Seq.empty[Transaction]
//              )
//            (prevHistory.append(block.header).right.get._1.append(block.payload).right.get._1.reportModifierIsValid(block),
//              Some(block), vector :+ block)
//        }
//    resultedHistory._1.closeStorage()
//
//    val blocks: Vector[Block] = resultedHistory._3
//  }
//}