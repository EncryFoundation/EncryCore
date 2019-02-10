package benches

import java.util.concurrent.TimeUnit

import benches.StateBench.BenchState
import encry.modifiers.history.Block
import org.openjdk.jmh.annotations._
import benches.Utils._
import encry.modifiers.state.box.AssetBox
import encry.settings.EncryAppSettings
import encry.view.state.{BoxHolder, UtxoState}
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}

class StateBench {

  @Benchmark
  def applyBlocksToTheState(stateBench: BenchState, bh: Blackhole): Unit = {
    bh.consume {
      val innerState: UtxoState =
        utxoFromBoxHolder(stateBench.boxesHolder.get, getRandomTempDir, None, stateBench.settings)
      stateBench.chain.foldLeft(innerState) { case (state, block) => state.applyModifier(block).get }
      innerState.closeStorage()
    }
  }
}

object StateBench {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[StateBench].getSimpleName + ".*")
      .forks(1)
      .threads(1)
      .warmupIterations(5)
      .measurementIterations(5)
      .mode(Mode.AverageTime)
      .timeUnit(TimeUnit.SECONDS)
      .addProfiler(classOf[GCProfiler])
      .verbosity(VerboseMode.EXTRA)
      .warmupTime(TimeValue.milliseconds(500))
      .measurementTime(TimeValue.milliseconds(500))
      .build
    new Runner(opt).run
  }

  @State(Scope.Benchmark)
  class BenchState {

    /**
      * Total number of boxes must be equal to total number of transactions.
      * (boxesNumber = blocksNumber * transactionsNumber).
      */
    val totalBoxesNumber: Int = 500000
    val blocksNumber: Int = 50000
    val transactionsNumberInEachBlock: Int = 10

    var initialBoxes: IndexedSeq[AssetBox] = IndexedSeq.empty[AssetBox]
    var boxesHolder: Option[BoxHolder] = None
    var state1: Option[UtxoState] = None
    var chain: Vector[Block] = Vector.empty[Block]
    val settings: EncryAppSettings = EncryAppSettings.read

    @Setup
    def createStateForBenchmark(): Unit = {
      initialBoxes = (0 until totalBoxesNumber).map(nonce => genHardcodedBox(privKey.publicImage.address.address, nonce))
      boxesHolder = Some(BoxHolder(initialBoxes))
      state1 = Some(utxoFromBoxHolder(boxesHolder.get, getRandomTempDir, None, settings))
      val genesisBlock: Block = generateGenesisBlockValidForState(state1.get)
      state1.get.applyModifier(genesisBlock)
      chain = genesisBlock +: (0 until blocksNumber).foldLeft(Vector[Block](), genesisBlock, state1.get, initialBoxes) {
        case ((vector, block, stateL, boxes), t) =>
          val nextBlock: Block = generateNextBlockValidForState(block, stateL, boxes.take(transactionsNumberInEachBlock))
          val stateN: UtxoState = stateL.applyModifier(nextBlock).get
          //if (t == blocksNumber - 1) stateN.closeStorage()
          (vector :+ nextBlock, nextBlock, stateN, boxes.drop(transactionsNumberInEachBlock))
      }._1
    }
  }
}