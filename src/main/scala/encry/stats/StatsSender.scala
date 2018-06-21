package encry.stats

import java.util

import akka.actor.Actor
import encry.EncryApp.{settings, timeProvider}
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.Algos
import encry.stats.StatsSender.{BestHeaderInChain, MiningEnd}
import encry.utils.ScorexLogging
import org.influxdb.{InfluxDB, InfluxDBFactory}

class StatsSender extends Actor with ScorexLogging {

  val influxDB: InfluxDB =
    InfluxDBFactory.connect(settings.influxDB.url, settings.influxDB.login, settings.influxDB.password )

  influxDB.setRetentionPolicy("autogen")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[BestHeaderInChain])
    influxDB.write(8189, util.Arrays.asList(
      s"nodesStartTime value=${settings.network.nodeName}"
    ))
  }

  override def receive: Receive = {

    case BestHeaderInChain(fb: EncryBlockHeader) =>

        influxDB.write(8189, util.Arrays.asList(
          s"difficulty,nodeName=${settings.network.nodeName} diff=${fb.difficulty.toString},height=${fb.height}",
          s"height,nodeName=${settings.network.nodeName},header=${Algos.encode(fb.id)} height=${fb.height}"
          //s"stateWeight,nodeName=${settings.network.nodeName},height=${fb.height} value=${new File("encry/data/state/journal-1").length}",
          //s"historyWeight,nodeName=${settings.network.nodeName},height=${fb.height} value=${FileUtils.sizeOfDirectory(new File("encry/data/history"))}",
          //s"lastHeaderSize,nodeName=${settings.network.nodeName} height=${fb.height},headerId=${Algos.encode(fb.id)},size=${fb.bytes.length}"
          )
        )

      //influxDB.write(8189, s"height,nodeName=${settings.network.nodeName} height=${fb.height},header=${Algos.encode(fb.id)}")

    case MiningEnd(blockHeader: EncryBlockHeader, workerNumber: Int) =>

      influxDB.write(8189, util.Arrays.asList(
        s"miningEnd,nodeName=${settings.network.nodeName},block=${Algos.encode(blockHeader.id)},height=${blockHeader.height},worker=$workerNumber value=${(timeProvider.time() - blockHeader.timestamp)/1000L}"
      ))
  }
}

object StatsSender {

  case class MiningEnd(blockHeader: EncryBlockHeader, workerNumber: Int)

  case class BestHeaderInChain(bestHeader: EncryBlockHeader)
}
