package benches

import java.io.File
import java.util.concurrent.TimeUnit

import BlockProto.BlockProtoMessage
import HeaderProto.HeaderProtoMessage
import PayloadProto.PayloadProtoMessage
import benches.DeserializeByProtoModifiersBench.DeserializeByProtoModifiersBenchState
import benches.SerializedByProtoModifiersBench.SerializedByProtoModifiersBenchState
import benches.Utils._
import encry.modifiers.history.{Block, BlockSerializer, HeaderSerializer, PayloadSerializer}
import encry.modifiers.state.box.AssetBox
import encry.settings.EncryAppSettings
import encry.view.state.{BoxHolder, UtxoState}
import encryBenchmark.Settings
import org.openjdk.jmh.annotations.{Benchmark, Mode, Scope, State}
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}

class DeserializeByProtoModifiersBench {

  @Benchmark
  def serializeHeaderByBytesBench(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume {
      stateBench.stateGenerationResults._1.map(x => HeaderSerializer.toBytes(x.header).length).sum
    }

  @Benchmark
  def serializePayloadByBytesBench(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume {
      stateBench.stateGenerationResults._1.map(x => PayloadSerializer.toBytes(x.payload).length).sum
    }

  @Benchmark
  def serializeBlockByBytesBench(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume {
      stateBench.stateGenerationResults._1.map(x => BlockSerializer.toBytes(x).length).sum
    }

  @Benchmark
  def deserializeHeaderByBytesBench(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume {
      stateBench.serHeadersBytes.map(x => HeaderSerializer.parseBytes(x))
    }

  @Benchmark
  def deserializePayloadByBytesBench(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume {
      stateBench.serPayloadBytes.map(x => PayloadSerializer.parseBytes(x))
    }

  @Benchmark
  def deserializeBlockByBytesBench(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume {
      stateBench.serBlockBytes.map(x => BlockSerializer.parseBytes(x))
    }

  @Benchmark
  def deserializeHeaderByProtoBench(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume {
      stateBench.serHeaders.map(x => HeaderSerializer.fromProto(x))
    }

  @Benchmark
  def deserializePayloadByProtoBench(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume {
      stateBench.serPayload.map(x => PayloadSerializer.fromProto(x))
    }

  @Benchmark
  def deserializeBlockByProtoBench(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume {
      stateBench.serBlocks.map(x => BlockSerializer.fromProto(x))
    }

  @Benchmark
  def serializeHeaderByProtoBench(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume(stateBench.stateGenerationResults._1.map(x => HeaderSerializer.toProto(x.header)))

  @Benchmark
  def serializePayloadByProtoBench(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume(stateBench.stateGenerationResults._1.map(x => PayloadSerializer.toProto(x.payload)))

  @Benchmark
  def serializeBlockByProtoBench(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume(stateBench.stateGenerationResults._1.map(x => BlockSerializer.toProto(x)))

  @Benchmark
  def deserializeHeaderByProtoBenchCalculate(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume {
      val a = stateBench.stateGenerationResults._1.map(x => HeaderSerializer.toProto(x.header).toByteArray.length).sum
      val a1 = stateBench.stateGenerationResults._1.map(x => HeaderSerializer.toBytes(x.header).length).sum
      println(s"Headers toProto: ${a / 1024} kBytes. toBytes ${a1 / 1024} kBytes.")
    }

  @Benchmark
  def deserializePayloadByProtoBenchCalculate(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume {
      val a = stateBench.stateGenerationResults._1.map(x => PayloadSerializer.toProto(x.payload).toByteArray.length).sum
      val a1 = stateBench.stateGenerationResults._1.map(x => PayloadSerializer.toBytes(x.payload).length).sum
      println(s"Payloads toProto: ${a / 1024} kBytes. toBytes ${a1 / 1024} kBytes.")
    }

  @Benchmark
  def deserializeBlockByProtoBenchCalculate(stateBench: DeserializeByProtoModifiersBenchState, bh: Blackhole): Unit =
    bh.consume {
      val a = stateBench.stateGenerationResults._1.map(x => BlockSerializer.toProto(x).toByteArray.length).sum
      val a1 = stateBench.stateGenerationResults._1.map(x => BlockSerializer.toBytes(x).length).sum
      println(s"Blocks toProto: ${a / 1024} kBytes. toBytes ${a1 / 1024} kBytes.")
    }

}

object DeserializeByProtoModifiersBench {

  val benchSettings: Settings = Settings.read

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[DeserializeByProtoModifiersBench].getSimpleName + ".*")
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
      .jvmArgsPrepend("-Djmh.shutdownTimeout=1")
      .build
    new Runner(opt).run
  }

  @State(Scope.Benchmark)
  class DeserializeByProtoModifiersBenchState {

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
    val serHeaders: Vector[HeaderProtoMessage] =
      stateGenerationResults._1.map(x => HeaderSerializer.toProto(x.header))
    val serPayload: Vector[PayloadProtoMessage] =
      stateGenerationResults._1.map(x => PayloadSerializer.toProto(x.payload))
    val serBlocks: Vector[BlockProtoMessage] =
      stateGenerationResults._1.map(x => BlockSerializer.toProto(x))

    val serHeadersBytes: Vector[Array[Byte]] =
      stateGenerationResults._1.map(x => HeaderSerializer.toBytes(x.header))
    val serPayloadBytes: Vector[Array[Byte]] =
      stateGenerationResults._1.map(x => PayloadSerializer.toBytes(x.payload))
    val serBlockBytes: Vector[Array[Byte]] =
      stateGenerationResults._1.map(x => BlockSerializer.toBytes(x))
    state.closeStorage()
  }

}