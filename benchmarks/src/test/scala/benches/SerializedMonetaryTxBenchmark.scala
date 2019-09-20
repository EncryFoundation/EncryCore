package benches

import java.util.concurrent.TimeUnit

import benches.SerializedMonetaryTxBenchmark.SerializedMonetaryBenchState
import encry.utils.ChainUtils._
import encryBenchmark.{BenchSettings, Settings}
import org.encryfoundation.common.modifiers.mempool.transaction.{Transaction, TransactionSerializer}
import org.encryfoundation.common.modifiers.state.box.AssetBox
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}

class SerializedMonetaryTxBenchmark {

  @Benchmark
  def deserializePaymentTransactionsBench(stateBench: SerializedMonetaryBenchState, bh: Blackhole): Unit =
    bh.consume(stateBench.serializedTransactions.map(b => TransactionSerializer.parseBytes(b)))

  @Benchmark
  def serializePaymentTransactionsBench(stateBench: SerializedMonetaryBenchState, bh: Blackhole): Unit =
    bh.consume(stateBench.initialTransactions.map(tx => tx.bytes))
}

object SerializedMonetaryTxBenchmark extends BenchSettings {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[SerializedMonetaryTxBenchmark].getSimpleName + ".*")
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
  class SerializedMonetaryBenchState {

    var initialBoxes: IndexedSeq[AssetBox] = IndexedSeq.empty[AssetBox]
    var initialTransactions: IndexedSeq[Transaction] = IndexedSeq.empty[Transaction]
    var serializedTransactions: IndexedSeq[Array[Byte]] = IndexedSeq.empty[Array[Byte]]

    @Setup
    def createStateForBenchmark(): Unit = {
      initialBoxes = generateInitialBoxes(benchSettings.serializedMonetaryBenchSettings.totalBoxesNumber)
      initialTransactions =
        generatePaymentTransactions(
          initialBoxes,
          benchSettings.serializedMonetaryBenchSettings.numberOfInputs,
          benchSettings.serializedMonetaryBenchSettings.numberOfOutputs
        )
      serializedTransactions = initialTransactions.map(tx => tx.bytes)
    }
  }
}