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
import org.openjdk.jmh.runner.options.OptionsBuilder

class StateBench {

  @Benchmark
  def applyBlocksToTheState(stateBench: BenchState, bh: Blackhole): Unit = {
    bh.consume {
      val innerState: UtxoState =
        utxoFromBoxHolder(stateBench.boxesHolder.get, getRandomTempDir, None, stateBench.settings)
      stateBench.chain.foldLeft(innerState) { case (state, block) => state.applyModifier(block).get }
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
      .warmupIterations(10)
      .measurementIterations(10)
      .mode(Mode.AverageTime)
      .timeUnit(TimeUnit.SECONDS)
      .addProfiler(classOf[GCProfiler])
      .build
    new Runner(opt).run
  }

  @State(Scope.Benchmark)
  class BenchState {

    val boxesNumber: Int = 100000
    val blocksNumber: Int = 1000
    val transactionsNumber: Int = 100

    var initialBoxes: IndexedSeq[AssetBox] = IndexedSeq.empty[AssetBox]
    var boxesHolder: Option[BoxHolder] = None
    var state1: Option[UtxoState] = None
    var chain: Vector[Block] = Vector.empty[Block]
    val settings: EncryAppSettings = EncryAppSettings.read

    @Setup
    def createStateForBenchmark(): Unit = {
      initialBoxes = (0 until boxesNumber).map(nonce => genHardcodedBox(privKey.publicImage.address.address, nonce))
      boxesHolder = Some(BoxHolder(initialBoxes))
      state1 = Some(utxoFromBoxHolder(boxesHolder.get, getRandomTempDir, None, settings))
      val genesisBlock: Block = generateGenesisBlock(state1.get)
      state1.get.applyModifier(genesisBlock)
      chain = genesisBlock +: (0 until blocksNumber).foldLeft(Vector[Block](), genesisBlock, state1.get, initialBoxes) {
        case ((vector, block, stateL, boxes), _) =>
          val nextBlock: Block = generateNextBlock(block, stateL, boxes.take(transactionsNumber))
          val stateN: UtxoState = stateL.applyModifier(nextBlock).get
          (vector :+ nextBlock, nextBlock, stateN, boxes.drop(transactionsNumber))
      }._1
    }
  }
}