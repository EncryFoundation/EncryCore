package benches

import java.io.File
import java.util.concurrent.TimeUnit

import benches.StateRollbackBench.StateRollbackState
import benches.Utils._
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.state.{BoxHolder, UtxoState}
import encryBenchmark.Settings
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.state.box.AssetBox
import org.encryfoundation.common.utils.TaggedTypes.Difficulty
import org.openjdk.jmh.annotations.{Benchmark, Mode, Scope, State}
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}

import scala.concurrent.Await
import scala.concurrent.duration._

class StateRollbackBench {

  @Benchmark
  def applyBlocksToTheState(stateBench: StateRollbackState, bh: Blackhole): Unit = {
    bh.consume {
      val innerState: UtxoState =
        utxoFromBoxHolder(stateBench.boxesHolder, getRandomTempDir, None, stateBench.settings, VersionalStorage.IODB)
      val newState = stateBench.chain.foldLeft(innerState -> List.empty[VersionTag]) { case ((state, rootHashes), block) =>
        val newState = Await.result(state.applyModifier(block), 100.seconds).right.get
        newState -> (rootHashes :+ newState.version)
      }
      val stateAfterRollback = newState._1.rollbackTo(newState._2.dropRight(1).last).get
      val stateAfterForkBlockApplying = Await.result(stateAfterRollback.applyModifier(stateBench.forkBlocks.last), 100.seconds).right.get
      stateAfterForkBlockApplying.close()
    }
  }
}

object StateRollbackBench {

  val benchSettings: Settings = Settings.read

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[StateRollbackBench].getSimpleName + ".*")
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
  class StateRollbackState {

    val settings: EncryAppSettings = EncryAppSettings.read
    val tmpDir: File = getRandomTempDir

    val initialBoxes: IndexedSeq[AssetBox] = (0 until benchSettings.stateBenchSettings.totalBoxesNumber).map(nonce =>
      genHardcodedBox(privKey.publicImage.address.address, nonce)
    )
    val boxesHolder: BoxHolder = BoxHolder(initialBoxes)
    var state: UtxoState = utxoFromBoxHolder(boxesHolder, tmpDir, None, settings, VersionalStorage.LevelDB)
    val genesisBlock: Block = generateGenesisBlockValidForState(state)

    state = Await.result(state.applyModifier(genesisBlock), 100.seconds).right.get

    val stateGenerationResults: (List[(Block, Block)], Block, UtxoState, IndexedSeq[AssetBox]) =
      (0 until benchSettings.stateBenchSettings.blocksNumber).foldLeft(List.empty[(Block, Block)], genesisBlock, state, initialBoxes) {
        case ((blocks, block, stateL, boxes), _) =>
          val nextBlockMainChain: Block = generateNextBlockForStateWithSpendingAllPreviousBoxes(
            block,
            stateL,
            block.payload.txs.flatMap(_.newBoxes.map(_.asInstanceOf[AssetBox])).toIndexedSeq
          )
          val nextBlockFork: Block = generateNextBlockForStateWithSpendingAllPreviousBoxes(
            block,
            stateL,
            block.payload.txs.flatMap(_.newBoxes.map(_.asInstanceOf[AssetBox])).toIndexedSeq,
            addDiff = Difficulty @@ BigInt(100)
          )
          val stateN: UtxoState = Await.result(stateL.applyModifier(nextBlockMainChain), 100.seconds).right.get
          (blocks :+ (nextBlockMainChain, nextBlockFork),
            nextBlockMainChain,
            stateN,
            boxes.drop(
              benchSettings.stateBenchSettings.transactionsNumberInEachBlock *
                benchSettings.stateBenchSettings.numberOfInputsInOneTransaction)
          )
      }
    val chain: List[Block] = genesisBlock +: stateGenerationResults._1.map(_._1)
    val forkBlocks: List[Block] = genesisBlock +: stateGenerationResults._1.map(_._2)
    state = stateGenerationResults._3
    state.close()
  }
}