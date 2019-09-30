//package benches
//
//import java.util.concurrent.TimeUnit
//import akka.actor.ActorSystem
//import benches.BloomFilterBenchmark.BloomFilterBenchmarkState
//import encry.modifiers.mempool.Transaction
//import encry.settings.EncryAppSettings
//import encry.utils.CoreTaggedTypes.ModifierId
//import encry.utils.NetworkTimeProvider
//import encry.view.mempool.Mempool
//import encryBenchmark.Settings
//import org.openjdk.jmh.annotations.{Benchmark, Mode, Scope, State}
//import org.openjdk.jmh.infra.Blackhole
//import org.openjdk.jmh.profile.GCProfiler
//import org.openjdk.jmh.runner.{Runner, RunnerException}
//import org.openjdk.jmh.runner.options.{OptionsBuilder, TimeValue, VerboseMode}
//
//class BloomFilterBenchmark {
//
//  @Benchmark
//  def put1000transactionsIntoBloomFilter(state: BloomFilterBenchmarkState, bh: Blackhole): Unit =
//    bh.consume {
//      val mempool: Mempool = Mempool.empty(state.settings, state.timeProvider, state.as)
//      state.getUniqTransactions(mempool, state.transactions1)
//      mempool.createBloomFilter
//    }
//
//  @Benchmark
//  def put10000transactionsIntoBloomFilter(state: BloomFilterBenchmarkState, bh: Blackhole): Unit =
//    bh.consume {
//      val mempool: Mempool = Mempool.empty(state.settings, state.timeProvider, state.as)
//      state.getUniqTransactions(mempool, state.transactions2)
//      mempool.createBloomFilter
//    }
//
//  @Benchmark
//  def put100000transactionsIntoBloomFilter(state: BloomFilterBenchmarkState, bh: Blackhole): Unit =
//    bh.consume {
//      val mempool: Mempool = Mempool.empty(state.settings, state.timeProvider, state.as)
//      state.getUniqTransactions(mempool, state.transactions3)
//      mempool.createBloomFilter
//    }
//}
//
//object BloomFilterBenchmark extends BenchSettings {
//
//  @throws[RunnerException]
//  def main(args: Array[String]): Unit = {
//    val opt = new OptionsBuilder()
//      .include(".*" + classOf[BloomFilterBenchmark].getSimpleName + ".*")
//      .forks(1)
//      .threads(1)
//      .warmupIterations(benchSettings.benchesSettings.warmUpIterations)
//      .measurementIterations(benchSettings.benchesSettings.measurementIterations)
//      .mode(Mode.AverageTime)
//      .timeUnit(TimeUnit.SECONDS)
//      .verbosity(VerboseMode.EXTRA)
//      .addProfiler(classOf[GCProfiler])
//      .warmupTime(TimeValue.milliseconds(benchSettings.benchesSettings.warmUpTime))
//      .measurementTime(TimeValue.milliseconds(benchSettings.benchesSettings.measurementTime))
//      .build
//    new Runner(opt).run
//  }
//
//  @State(Scope.Benchmark)
//  class BloomFilterBenchmarkState {
//    val settings: EncryAppSettings = EncryAppSettings.read
//    val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.ntp)
//    val as: ActorSystem = ActorSystem()
//
//    val transactions1: Seq[Transaction] = genValidPaymentTxs(1000)
//    val transactions2: Seq[Transaction] = genValidPaymentTxs(10000)
//    val transactions3: Seq[Transaction] = genValidPaymentTxs(100000)
//
//    def getUniqTransactions(mempool: Mempool, transactions: Seq[Transaction]): Unit = transactions.foreach { tx =>
//      val filteredTxs: Seq[ModifierId] = mempool.notRequested(Seq(tx.id))
//      if (filteredTxs.nonEmpty) {
//        mempool.put(tx)
//      }
//    }
//  }
//
//}