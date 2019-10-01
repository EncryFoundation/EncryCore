package benches

import java.io.File
import java.util.concurrent.TimeUnit

import benches.StateRollbackBench.StateRollbackState
import encry.modifiers.mempool.TransactionFactory.paymentTransactionWithMultipleOutputs
import encry.settings.Settings
import encry.storage.VersionalStorage
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.state.{BoxHolder, UtxoState}
import encryBenchmark.BenchSettings
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.modifiers.state.box.AssetBox
import org.encryfoundation.common.utils.TaggedTypes.Difficulty
import org.openjdk.jmh.annotations.{Benchmark, Mode, Scope, State}
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}
import encry.utils.FileHelper.getRandomTempDir
import encry.utils.ChainGenerator.{genGenesisBlock, utxoFromBoxHolder}
import encry.utils.{Keys, TestEntityGenerator}
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction

import scala.util.Random

class StateRollbackBench {

  @Benchmark
  def applyBlocksToTheState(stateBench: StateRollbackState, bh: Blackhole): Unit = {
    bh.consume {
      val innerState: UtxoState =
        utxoFromBoxHolder(stateBench.boxesHolder, getRandomTempDir, stateBench.settings, VersionalStorage.IODB)
      val newState = stateBench.chain.foldLeft(innerState -> List.empty[VersionTag]) { case ((state, rootHashes), block) =>
        val newState = state.applyModifier(block).right.get
        newState -> (rootHashes :+ newState.version)
      }
      val stateAfterRollback = newState._1.rollbackTo(newState._2.dropRight(1).last).get
      val stateAfterForkBlockApplying = stateAfterRollback.applyModifier(stateBench.forkBlocks.last).right.get
      stateAfterForkBlockApplying.close()
    }
  }
}

object StateRollbackBench extends BenchSettings {

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
  class StateRollbackState extends Settings with Keys {

    val tmpDir: File = getRandomTempDir

    val initialBoxes: IndexedSeq[AssetBox] = (0 until benchSettings.stateBenchSettings.totalBoxesNumber).map(nonce =>
      TestEntityGenerator.genHardcodedBox(privKey.publicImage.address.address, nonce)
    )
    val boxesHolder: BoxHolder = BoxHolder(initialBoxes)
    var state: UtxoState = utxoFromBoxHolder(boxesHolder, tmpDir, settings, VersionalStorage.LevelDB)
    val genesisBlock: Block = genGenesisBlock(privKey.publicImage, settings.constants.InitialEmissionAmount,
      settings.constants.InitialDifficulty, settings.constants.GenesisHeight)

    state = state.applyModifier(genesisBlock).right.get

    val stateGenerationResults: (List[(Block, Block)], Block, UtxoState, IndexedSeq[AssetBox]) =
      (0 until benchSettings.stateBenchSettings.blocksNumber).foldLeft(List.empty[(Block, Block)], genesisBlock, state, initialBoxes) {
        case ((blocks, block, stateL, boxes), _) =>
          val nextBlockMainChain: Block = generateNextBlockForStateWithSpendingAllPreviousBoxes(
            block,
            stateL,
            block.payload.txs.flatMap(_.newBoxes.map(_.asInstanceOf[AssetBox])).toIndexedSeq)
          val nextBlockFork: Block = generateNextBlockForStateWithSpendingAllPreviousBoxes(
            block,
            stateL,
            block.payload.txs.flatMap(_.newBoxes.map(_.asInstanceOf[AssetBox])).toIndexedSeq,
            addDiff = Difficulty @@ BigInt(100)
          )
          val stateN: UtxoState = stateL.applyModifier(nextBlockMainChain).right.get
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

    def generateNextBlockForStateWithSpendingAllPreviousBoxes(prevBlock: Block,
                                                              state: UtxoState,
                                                              box: Seq[AssetBox],
                                                              splitCoef: Int = 2,
                                                              addDiff: Difficulty = Difficulty @@ BigInt(0)): Block = {

      val transactions: Seq[Transaction] = box.indices.foldLeft(box, Seq.empty[Transaction]) {
        case ((boxes, transactionsL), _) =>
          val tx: Transaction = paymentTransactionWithMultipleOutputs(
            privKey,
            fee = 1,
            timestamp = 11L,
            useBoxes = IndexedSeq(boxes.head),
            recipient = privKey.publicImage.address.address,
            amount = boxes.head.amount - 1,
            numOfOutputs = splitCoef
          )
          (boxes.tail, transactionsL :+ tx)
      }._2.filter(tx => state.validate(tx).isRight) ++ Seq(TestEntityGenerator.coinbaseTransaction(prevBlock.header.height + 1))
      val header = Header(
        1.toByte,
        prevBlock.id,
        Payload.rootHash(transactions.map(_.id)),
        System.currentTimeMillis(),
        prevBlock.header.height + 1,
        Random.nextLong(),
        Difficulty @@ (BigInt(1) + addDiff),
        EquihashSolution(Seq(1, 3))
      )
      Block(header, Payload(header.id, transactions))
    }
  }

}