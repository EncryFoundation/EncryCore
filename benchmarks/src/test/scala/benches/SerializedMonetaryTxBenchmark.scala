package benches

import java.util.concurrent.TimeUnit
import benches.SerializedMonetaryTxBenchmark.SerializedMonetaryBenchState
import benches.Utils._
import benches.Utils.privKey
import encry.modifiers.mempool.{Transaction, TransactionSerializer}
import encry.modifiers.state.box.AssetBox
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}

class SerializedMonetaryTxBenchmark {

  @Benchmark
  def applyBlocksToTheState(stateBench: SerializedMonetaryBenchState, bh: Blackhole): Unit = {
    bh.consume {
      val result = stateBench.serializedTransactions.map(b => TransactionSerializer.parseBytes(b))
      result
    }
  }
}

object SerializedMonetaryTxBenchmark {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[SerializedMonetaryTxBenchmark].getSimpleName + ".*")
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
  class SerializedMonetaryBenchState {
    val totalBoxesNumber: Int = 500000
    var initialBoxes: IndexedSeq[AssetBox] = IndexedSeq.empty[AssetBox]
    var initialTransactions: IndexedSeq[Transaction] = IndexedSeq.empty[Transaction]
    var serializedTransactions: IndexedSeq[Array[Byte]] = IndexedSeq.empty[Array[Byte]]
    val numberOfInputs: Int = 50

    @Setup
    def createStateForBenchmark(): Unit = {
      initialBoxes = (0 until totalBoxesNumber).map(_ => genAssetBox(privKey.publicImage.address.address))

      (0 until totalBoxesNumber / numberOfInputs).foldLeft(initialBoxes) { case (boxes, _) =>
        val tx: Transaction = defaultPaymentTransactionScratch(
          privKey,
          fee = 111,
          timestamp = 11L,
          useBoxes = boxes.take(numberOfInputs),
          recipient = randomAddress,
          amount = 10000,
          numOfOutputs = 200
        )
        initialTransactions = tx +: initialTransactions
        boxes.drop(numberOfInputs)
      }
      serializedTransactions = initialTransactions.map(tx => tx.bytes)
    }
  }
}