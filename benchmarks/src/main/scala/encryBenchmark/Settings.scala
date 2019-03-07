package encryBenchmark

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class Settings(serializedAssetBenchSettings: SerializedAssetBenchSettings,
                    serializedMonetaryBenchSettings: SerializedMonetaryBenchSettings,
                    serializedDataBenchSettings: SerializedDataBenchSettings,
                    benchesSettings: BenchesSettings,
                    stateBenchSettings: StateBenchSettings,
                    historyBenchSettings: HistoryBenchSettings)

object Settings {
  val configPath = "encry.benchmark"
  val read: Settings = ConfigFactory.load("application.conf").as[Settings](configPath)
}

case class SerializedAssetBenchSettings(totalBoxesNumber: Int, numberOfInputs: Int, numberOfOutputs: Int)

case class SerializedMonetaryBenchSettings(totalBoxesNumber: Int, numberOfInputs: Int, numberOfOutputs: Int)

case class SerializedDataBenchSettings(totalBoxesNumber: Int, numberOfInputs: Int, numberOfOutputs: Int, bytesQty: Int)

case class BenchesSettings(warmUpIterations: Int, measurementIterations: Int, measurementTime: Int, warmUpTime: Int)

case class StateBenchSettings(totalBoxesNumber: Int,
                              blocksNumber: Int,
                              transactionsNumberInEachBlock: Int,
                              numberOfInputsInOneTransaction: Int,
                              numberOfOutputsInOneTransaction: Int)

case class HistoryBenchSettings(blocksNumber: Int, transactionsNumber: Int)