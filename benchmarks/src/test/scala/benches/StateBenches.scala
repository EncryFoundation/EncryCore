package benches

import java.io.File
import java.util.concurrent.TimeUnit
import benches.StateBenches.StateBenchState
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

class StateBenches {

  @Benchmark
  def applyBlocksToTheState(stateBench: StateBenchState, bh: Blackhole): Unit = {
    bh.consume {
      val innerState: UtxoState =
        utxoFromBoxHolder(stateBench.boxesHolder, getRandomTempDir, None, stateBench.settings)
      stateBench.chain.foldLeft(innerState) { case (state, block) => state.applyModifier(block).get }
      innerState.closeStorage()
    }
  }

  @Benchmark
  def readStateFileBench(stateBench: StateBenchState, bh: Blackhole): Unit = {
    bh.consume {
      val localState: UtxoState =
        utxoFromBoxHolder(stateBench.boxesHolder, stateBench.tmpDir, None, stateBench.settings)
      localState.closeStorage()
    }
  }
}

object StateBenches {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[StateBenches].getSimpleName + ".*")
      .forks(1)
      .threads(1)
      .warmupIterations(10)
      .measurementIterations(10)
      .mode(Mode.AverageTime)
      .timeUnit(TimeUnit.SECONDS)
      .addProfiler(classOf[GCProfiler])
      .verbosity(VerboseMode.EXTRA)
      .warmupTime(TimeValue.milliseconds(500))
      .measurementTime(TimeValue.minutes(5))
      .build
    new Runner(opt).run
  }

  @State(Scope.Benchmark)
  class StateBenchState {

    /**
      * (totalBoxesNumber >= blocksNumber * transactionsNumberInEachBlock * numberOfInputsInOneTransaction)!
      */
    val totalBoxesNumber: Int = 5000
    val blocksNumber: Int = 10
    val transactionsNumberInEachBlock: Int = 25
    val numberOfInputsInOneTransaction: Int = 20
    val numberOfOutputsInOneTransaction: Int = 100

    val settings: EncryAppSettings = EncryAppSettings.read
    val tmpDir: File = getRandomTempDir

    val initialBoxes: IndexedSeq[AssetBox] = (0 until totalBoxesNumber).map(nonce =>
      genHardcodedBox(privKey.publicImage.address.address, nonce)
    )
    val boxesHolder: BoxHolder = BoxHolder(initialBoxes)
    var state: UtxoState = utxoFromBoxHolder(boxesHolder, tmpDir, None, settings)
    val genesisBlock: Block = generateGenesisBlockValidForState(state)

    state = state.applyModifier(genesisBlock).get

    val stateGenerationResults: (Vector[Block], Block, UtxoState, IndexedSeq[AssetBox]) =
      (0 until blocksNumber).foldLeft(Vector[Block](), genesisBlock, state, initialBoxes) {
        case ((blocks, block, stateL, boxes), _) =>
          val nextBlock: Block = generateNextBlockValidForState(
            block,
            stateL,
            boxes.take(transactionsNumberInEachBlock * numberOfInputsInOneTransaction),
            transactionsNumberInEachBlock,
            numberOfInputsInOneTransaction,
            numberOfOutputsInOneTransaction
          )
          val stateN: UtxoState = stateL.applyModifier(nextBlock).get
          (blocks :+ nextBlock, nextBlock, stateN, boxes.drop(transactionsNumberInEachBlock * numberOfInputsInOneTransaction))
      }

    val chain: Vector[Block] = genesisBlock +: stateGenerationResults._1
    state = stateGenerationResults._3
    state.closeStorage()
  }
}