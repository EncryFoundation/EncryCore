package benches

import java.io.File
import java.util.concurrent.TimeUnit

import benches.StateBenches.StateBenchState
import org.openjdk.jmh.annotations._
import benches.Utils._
import encry.EncryApp
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.IODB
import encry.utils.{ChainGenerator, FileHelper}
import encry.utils.ChainGenerator._
import encry.utils.FileHelper.getRandomTempDir
import encry.view.state.{BoxHolder, UtxoState}
import encryBenchmark.{BenchSettings, Settings}
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.state.box.AssetBox
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}

class StateBenches {

  @Benchmark
  def applyBlocksToTheState(stateBench: StateBenchState, bh: Blackhole): Unit = {
    bh.consume {
      val innerState: UtxoState =
         utxoFromBoxHolder(stateBench.boxesHolder, getRandomTempDir, None, stateBench.settings, VersionalStorage.LevelDB)
      stateBench.chain.foldLeft(innerState) { case (state, block) =>
        state.applyModifier(block).right.get
      }
      innerState.close()
    }
  }

  @Benchmark
  def readStateFileBench(stateBench: StateBenchState, bh: Blackhole): Unit = {
    bh.consume {
      val localState: UtxoState =
        utxoFromBoxHolder(stateBench.boxesHolder, stateBench.tmpDir, None, stateBench.settings, IODB)
      localState.close()
    }
  }
}

object StateBenches extends BenchSettings {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[StateBenches].getSimpleName + ".*")
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
  class StateBenchState extends encry.settings.Settings {

    val tmpDir: File = getRandomTempDir

    val initialBoxes: IndexedSeq[AssetBox] = (0 until benchSettings.stateBenchSettings.totalBoxesNumber).map(nonce =>
      genHardcodedBox(privKey.publicImage.address.address, nonce)
    )
    val boxesHolder: BoxHolder = BoxHolder(initialBoxes)
    var state: UtxoState = utxoFromBoxHolder(boxesHolder, tmpDir, None, settings, VersionalStorage.LevelDB)
    val genesisBlock: Block = generateGenesisBlockValidForState(state)

    state = state.applyModifier(genesisBlock).right.get

    val stateGenerationResults: (Vector[Block], Block, UtxoState, IndexedSeq[AssetBox]) =
      (0 until benchSettings.stateBenchSettings.blocksNumber).foldLeft(Vector[Block](), genesisBlock, state, initialBoxes) {
        case ((blocks, block, stateL, boxes), _) =>
          val nextBlock: Block = generateNextBlockValidForState(
            block,
            stateL,
            boxes.take(
              benchSettings.stateBenchSettings.transactionsNumberInEachBlock *
                benchSettings.stateBenchSettings.numberOfInputsInOneTransaction
            ),
            benchSettings.stateBenchSettings.transactionsNumberInEachBlock,
            benchSettings.stateBenchSettings.numberOfInputsInOneTransaction,
            benchSettings.stateBenchSettings.numberOfOutputsInOneTransaction
          )
          val stateN: UtxoState = stateL.applyModifier(nextBlock).right.get
          (blocks :+ nextBlock,
            nextBlock,
            stateN,
            boxes.drop(
              benchSettings.stateBenchSettings.transactionsNumberInEachBlock *
                benchSettings.stateBenchSettings.numberOfInputsInOneTransaction)
          )
      }

    val chain: Vector[Block] = genesisBlock +: stateGenerationResults._1
    state = stateGenerationResults._3
    state.close()
  }
}