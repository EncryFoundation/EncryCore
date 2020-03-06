package benches

import java.io.File
import java.util.concurrent.TimeUnit

import benches.SnapshotAssemblerBench.SnapshotAssemblerBenchState
import encry.view.state.avlTree.utils.implicits.Instances._
import benches.StateBenches.{StateBenchState, benchSettings}
import benches.Utils.{getRandomTempDir, utxoFromBoxHolder}
import encry.nvg.fast.sync.SnapshotProcessor
import encry.settings.Settings
import encry.storage.{RootNodesStorage, VersionalStorage}
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.utils.FileHelper
import encry.view.state.UtxoState
import encry.view.state.avlTree.AvlTree
import org.encryfoundation.common.utils.TaggedTypes.Height
import org.iq80.leveldb.{DB, Options}
import org.openjdk.jmh.annotations.{Benchmark, Mode, Scope, State}
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.profile.GCProfiler
import org.openjdk.jmh.runner.{Runner, RunnerException}
import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}
import scorex.utils.Random

class SnapshotAssemblerBench {

  /*
  1000 boxes = 0.025s
  10000 boxes = 0.75s
  500000 boxes = 225s
   */
  @Benchmark
  def createTree(stateBench: SnapshotAssemblerBenchState, bh: Blackhole): Unit = {
    bh.consume {
      //stateBench.a.initializeSnapshotData(stateBench.block1)
    }
  }
}
object SnapshotAssemblerBench {

  @throws[RunnerException]
  def main(args: Array[String]): Unit = {
    val opt = new OptionsBuilder()
      .include(".*" + classOf[SnapshotAssemblerBench].getSimpleName + ".*")
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
  class SnapshotAssemblerBenchState extends Settings {

    val a: AvlTree[StorageKey, StorageValue] =
      createAvl("9gKDVmfsA6J4b78jDBx6JmS86Zph98NnjnUqTJBkW7zitQMReia", 0, 500000)
    val block1                              = Utils.generateGenesisBlock(Height @@ 1)


    def createAvl(address: String, from: Int, to: Int): AvlTree[StorageKey, StorageValue] = {
      val firstDir: File = FileHelper.getRandomTempDir
      val firstStorage: VLDBWrapper = {
        val levelDBInit = LevelDbFactory.factory.open(firstDir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
      }
      val dir: File = FileHelper.getRandomTempDir
      val levelDb: DB = LevelDbFactory.factory.open(dir, new Options)
      val rootNodesStorage = RootNodesStorage[StorageKey, StorageValue](levelDb, 10, dir)

      val firstAvl: AvlTree[StorageKey, StorageValue] = AvlTree[StorageKey, StorageValue](firstStorage, rootNodesStorage)
      val avlNew = (from to to).foldLeft(firstAvl) { case (avl, i) =>
        val bx = Utils.genAssetBox(address, i, nonce = i)
        val b = (StorageKey !@@ bx.id, StorageValue @@ bx.bytes)
        avl.insertAndDeleteMany(StorageVersion @@ Random.randomBytes(), List(b), List.empty)
      }
      avlNew
    }

    def tmpDir: File = FileHelper.getRandomTempDir
  }

}
