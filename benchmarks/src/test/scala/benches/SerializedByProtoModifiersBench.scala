package benches

import java.io.File
import java.util.concurrent.TimeUnit

import benches.SerializedByProtoModifiersBench.SerializedByProtoModifiersBenchState
import benches.SerializedDataTxBenchmark.SerializedDataBenchState
import benches.StateBenches.benchSettings
import benches.Utils._
import encry.modifiers.history.{Block, BlockSerializer, HeaderSerializer, PayloadSerializer}
import encry.modifiers.mempool.TransactionSerializer
import encry.modifiers.state.box.AssetBox
import encry.settings.EncryAppSettings
import encry.view.state.{BoxHolder, UtxoState}
import encryBenchmark.Settings
import org.openjdk.jmh.annotations.{Benchmark, Mode, Scope, State}
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}

class SerializedByProtoModifiersBench {

  @Benchmark
  def serializeHeaderByProtoBench(stateBench: SerializedByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume {
      val a1 = stateBench.stateGenerationResults._1.map(x => HeaderSerializer.toProto(x.header).toByteArray.length).sum
      val a2 = stateBench.stateGenerationResults._1.map(x => PayloadSerializer.toProto(x.payload).toByteArray.length).sum
      val a3 = stateBench.stateGenerationResults._1.map(x => BlockSerializer.toProto(x).toByteArray.length).sum

      val a11 = stateBench.stateGenerationResults._1.map(x => HeaderSerializer.toBytes(x.header).length).sum
      val a22 = stateBench.stateGenerationResults._1.map(x => PayloadSerializer.toBytes(x.payload).length).sum
      val a33 = stateBench.stateGenerationResults._1.map(x => BlockSerializer.toBytes(x).length).sum
      println(s"PROTOBUF headers size - ${a1 / 1024} kB, payloads - ${a2 / 1024} kB, blocks - ${a3 / 1024} kB.")
      println(s"BYTES headers size - ${a11 / 1024} kB, payloads - ${a22 / 1024} kB, blocks - ${a33 / 1024} kB.")
    }


}

object SerializedByProtoModifiersBench {

  val benchSettings: Settings = Settings.read

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[SerializedByProtoModifiersBench].getSimpleName + ".*")
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
  class SerializedByProtoModifiersBenchState {

    val settings: EncryAppSettings = EncryAppSettings.read
    val tmpDir: File = getRandomTempDir

    var initialBoxes: IndexedSeq[AssetBox] = (0 until benchSettings.stateBenchSettings.totalBoxesNumber).map(nonce =>
      genHardcodedBox(privKey.publicImage.address.address, nonce)
    )
    val boxesHolder: BoxHolder = BoxHolder(initialBoxes)
    var state: UtxoState = utxoFromBoxHolder(boxesHolder, tmpDir, None, settings)
    val genesisBlock: Block = generateGenesisBlockValidForState(state)

    state = state.applyModifier(genesisBlock).get

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
          val stateN: UtxoState = stateL.applyModifier(nextBlock).get
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
    initialBoxes = IndexedSeq.empty
    state.closeStorage()
  }

}