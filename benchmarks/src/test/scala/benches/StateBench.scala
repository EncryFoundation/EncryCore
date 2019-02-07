package benches

import java.util.concurrent.TimeUnit
import benches.StateBench.BenchState
import encry.modifiers.history.Block
import org.openjdk.jmh.annotations._
import benches.Utils._
import encry.view.state.{BoxHolder, UtxoState}
import org.encryfoundation.common.utils.TaggedTypes.ADDigest
import org.openjdk.jmh.infra.Blackhole
import scorex.utils.Random

@OutputTimeUnit(TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 15)
@Measurement(iterations = 20)
class StateBench {

  @Benchmark
  def applyBlocksToTheState(stateBench: BenchState, bh: Blackhole): Unit = {
    bh.consume {
      stateBench.blocks.foreach { block =>
        stateBench.state.get.applyBlockTransactions(
          block.payload.transactions,
          ADDigest @@ Random.randomBytes(33)
        )
      }
    }
  }
}

object StateBench {

  @State(Scope.Benchmark)
  class BenchState {

    var state: Option[UtxoState] = None

    var blocks: Vector[Block] = Vector[Block]()

    @Setup
    def generatedState(): Unit = {
      val bh: BoxHolder = BoxHolder(Seq())
      state = Some(utxoFromBoxHolder(bh, getRandomTempDir, None))
    }

    @Setup
    def generateNBlocks(): Unit = {
      val genesisBlock: Block = generateGenesisBlock
      blocks = genesisBlock +: (0 to 1000).foldLeft(Vector[Block](), genesisBlock) {
        case ((list, block), _) =>
          val nextBlock: Block = generateNextBlock(block)
          (nextBlock +: list, nextBlock)
      }._1
    }
  }
}