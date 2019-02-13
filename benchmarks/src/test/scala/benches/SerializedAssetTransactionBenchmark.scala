package benches

import java.util.concurrent.TimeUnit
import benches.SerializedAssetTransactionBenchmark.SerializedAssetBenchState
import benches.Utils._
import encry.modifiers.mempool.{Transaction, TransactionSerializer}
import encry.modifiers.state.box.AssetBox
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}

class SerializedAssetTransactionBenchmark {

  @Benchmark
  def deserializeDataTransactionsBench(stateBench: SerializedAssetBenchState, bh: Blackhole): Unit =
    bh.consume(stateBench.serializedTransactions.map(b => TransactionSerializer.parseBytes(b)))

  @Benchmark
  def serializeDataTransactionsBench(stateBench: SerializedAssetBenchState, bh: Blackhole): Unit =
    bh.consume(stateBench.initialTransactions.map(tx => tx.bytes))
}

object SerializedAssetTransactionBenchmark {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[SerializedAssetTransactionBenchmark].getSimpleName + ".*")
      .forks(1)
      .threads(1)
      .warmupIterations(10)
      .measurementIterations(10)
      .mode(Mode.AverageTime)
      .timeUnit(TimeUnit.SECONDS)
      .verbosity(VerboseMode.EXTRA)
      .addProfiler(classOf[GCProfiler])
      .warmupTime(TimeValue.milliseconds(500))
      .measurementTime(TimeValue.milliseconds(500))
      .build
    new Runner(opt).run
  }

  @State(Scope.Benchmark)
  class SerializedAssetBenchState {
    val totalBoxesNumber: Int = 100000
    val numberOfInputs: Int = 25
    val numberOfOutputs: Int = 25

    var initialBoxes: IndexedSeq[AssetBox] = IndexedSeq.empty[AssetBox]
    var initialTransactions: IndexedSeq[Transaction] = IndexedSeq.empty[Transaction]
    var serializedTransactions: IndexedSeq[Array[Byte]] = IndexedSeq.empty[Array[Byte]]

    @Setup
    def createStateForBenchmark(): Unit = {
      initialBoxes = generateInitialBoxes(totalBoxesNumber)
      initialTransactions = generateAssetTransactions(initialBoxes, numberOfInputs, numberOfOutputs)
      serializedTransactions = initialTransactions.map(tx => tx.bytes)
    }
  }
}