package benches

import java.util.concurrent.TimeUnit
import benches.SerializedBenchmark.SerializedBenchState
import benches.Utils._
import benches.Utils.privKey
import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.mempool.{Transaction, TransactionSerializer}
import encry.modifiers.state.box.AssetBox
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}

class SerializedBenchmark {

  @Benchmark
  def applyBlocksToTheState(stateBench: SerializedBenchState, bh: Blackhole): Unit = {
    bh.consume {
      val result = stateBench.serializedTransactions.map(b => TransactionSerializer.parseBytes(b))
      result
    }
  }
}

object SerializedBenchmark extends StrictLogging {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[SerializedBenchmark].getSimpleName + ".*")
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
  class SerializedBenchState {
    val totalBoxesNumber: Int = 500000
    var initialBoxes: IndexedSeq[AssetBox] = IndexedSeq.empty[AssetBox]
    var initialTransactions: IndexedSeq[Transaction] = IndexedSeq.empty[Transaction]
    var serializedTransactions: IndexedSeq[Array[Byte]] = IndexedSeq.empty[Array[Byte]]
    val numberOfInputs: Int = 50

    @Setup
    def createStateForBenchmark(): Unit = {
      initialBoxes = (0 until totalBoxesNumber).map(_ => genAssetBox(privKey.publicImage.address.address))

      (0 until totalBoxesNumber / numberOfInputs).foldLeft(initialBoxes) { case (boxes, _) =>
        val tx: Transaction = createTxForTxSerializerWithInputsOutputs(
          privKey,
          fee = 111,
          timestamp = 11L,
          useBoxes = boxes.take(numberOfInputs),
          recipient = randomAddress,
          amount = 10000
        )
        initialTransactions = tx +: initialTransactions
        boxes.drop(numberOfInputs)
      }
      serializedTransactions = initialTransactions.map(tx => tx.bytes)
    }
  }
}