package benches

import java.util.concurrent.TimeUnit
import benches.SerializedDataTxBenchmark.SerializedDataBenchState
import benches.Utils._
import encry.modifiers.mempool.{Transaction, TransactionSerializer}
import encry.modifiers.state.box.AssetBox
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}
import scorex.utils.Random

class SerializedDataTxBenchmark {

  @Benchmark
  def applyBlocksToTheState(stateBench: SerializedDataBenchState, bh: Blackhole): Unit = {
    bh.consume {
      val result = stateBench.serializedTransactions.map(b => TransactionSerializer.parseBytes(b))
      result
    }
  }

}

object SerializedDataTxBenchmark {

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
  class SerializedDataBenchState {
    val totalBoxesNumber: Int = 100000
    var initialBoxes: IndexedSeq[AssetBox] = IndexedSeq.empty[AssetBox]
    var initialTransactions: IndexedSeq[Transaction] = IndexedSeq.empty[Transaction]
    var serializedTransactions: IndexedSeq[Array[Byte]] = IndexedSeq.empty[Array[Byte]]
    val numberOfInputs: Int = 5

    @Setup
    def createStateForBenchmark(): Unit = {
      initialBoxes = (0 until totalBoxesNumber).map(_ => genAssetBox(privKey.publicImage.address.address))

      (0 until totalBoxesNumber / numberOfInputs).foldLeft(initialBoxes) { case (boxes, _) =>
        val tx: Transaction = dataTransactionScratch(
          privKey,
          fee = 111,
          timestamp = 11L,
          useOutputs = boxes.take(numberOfInputs),
          data = Random.randomBytes(900),
          amount = 200L,
          numOfOutputs = 100
        )
        initialTransactions = tx +: initialTransactions
        boxes.drop(numberOfInputs)
      }
      serializedTransactions = initialTransactions.map(tx => tx.bytes)
    }
  }
}