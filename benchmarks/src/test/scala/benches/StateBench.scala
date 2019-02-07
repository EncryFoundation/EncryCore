package benches

import java.util.concurrent.TimeUnit
import benches.StateBench.BenchState
import encry.modifiers.history.Block
import org.openjdk.jmh.annotations._
import benches.Utils._
import encry.modifiers.state.box.AssetBox
import encry.view.state.{BoxHolder, UtxoState}
import org.encryfoundation.common.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, SerializedAdProof}
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.OptionsBuilder
import scala.collection.immutable

class StateBench {

  @Benchmark
  def applyBlocksToTheState(stateBench: BenchState, bh: Blackhole): Unit = {
    bh.consume {
      stateBench.blocks.foldLeft(stateBench.state2) { case (state, block) =>
        //val (_: SerializedAdProof, adDigest: ADDigest) = state.generateProofs(block.transactions).get
        println(1)
        state.applyModifier(block).get
      }
    }
  }
}

object StateBench {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[StateBench].getSimpleName + ".*")
      .forks(1)
      .threads(4)
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

    val boxesNumber: Int = 10000

    val initialBoxes: IndexedSeq[AssetBox] = (0 to boxesNumber).map(_ =>
      genHardcodedBox(privKey.publicImage.address.address)
    )

    var state: UtxoState = generatedState

    var blocks: Vector[Block] = Vector[Block]()

    var state2: UtxoState = generatedState

    def generatedState: UtxoState = {
      val bh: BoxHolder = BoxHolder(initialBoxes)
      utxoFromBoxHolder(bh, getRandomTempDir, None)
    }

    @Setup
    def generateNBlocks(): Unit = {
      val genesisBlock: Block = generateGenesisBlock
      blocks = genesisBlock +: (0 until 100).foldLeft(Vector[Block](), genesisBlock, state, initialBoxes) {
        case ((list, block, stateL, boxes), _) =>
          val nextBlock: Block = generateNextBlock(block, stateL, boxes.take(100))
          val stateN: UtxoState = stateL.applyModifier(block).get
          (nextBlock +: list, nextBlock, stateN, boxes.drop(100))
      }._1
    }
  }

}