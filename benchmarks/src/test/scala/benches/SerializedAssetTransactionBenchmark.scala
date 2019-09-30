package benches

import java.util.concurrent.TimeUnit

import benches.SerializedAssetTransactionBenchmark.SerializedAssetBenchState
import encry.utils.TestEntityGenerator
import encryBenchmark.BenchSettings
import org.encryfoundation.common.modifiers.mempool.transaction.{Transaction, TransactionSerializer}
import org.encryfoundation.common.modifiers.state.box.AssetBox
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}

class SerializedAssetTransactionBenchmark {

  @Benchmark
  def deserializeAssetTransactionsBench(stateBench: SerializedAssetBenchState, bh: Blackhole): Unit =
    bh.consume(stateBench.serializedTransactions.map(b => TransactionSerializer.parseBytes(b)))

  @Benchmark
  def serializeAssetTransactionsBench(stateBench: SerializedAssetBenchState, bh: Blackhole): Unit =
    bh.consume(stateBench.initialTransactions.map(tx => tx.bytes))
}

object SerializedAssetTransactionBenchmark extends BenchSettings {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[SerializedAssetTransactionBenchmark].getSimpleName + ".*")
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
  class SerializedAssetBenchState {

    var initialBoxes: IndexedSeq[AssetBox] = IndexedSeq.empty[AssetBox]
    var initialTransactions: IndexedSeq[Transaction] = IndexedSeq.empty[Transaction]
    var serializedTransactions: IndexedSeq[Array[Byte]] = IndexedSeq.empty[Array[Byte]]

    @Setup
    def createStateForBenchmark(): Unit = {
      initialBoxes = TestEntityGenerator.generateInitialBoxes(benchSettings.serializedAssetBenchSettings.totalBoxesNumber)
      initialTransactions =
        TestEntityGenerator.generateAssetTransactions(
          initialBoxes,
          benchSettings.serializedAssetBenchSettings.numberOfInputs,
          benchSettings.serializedAssetBenchSettings.numberOfOutputs
        )
      serializedTransactions = initialTransactions.map(tx => tx.bytes)
    }
  }
}