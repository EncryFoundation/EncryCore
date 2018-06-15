package encry.stats

import java.io.File
import java.util

import akka.actor.Actor
import encry.EncryApp.{settings, timeProvider}
import encry.consensus.DifficultySerializer
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.settings.Algos
import encry.stats.StatsSender.MiningEnd
import encry.utils.ScorexLogging
import org.apache.commons.io.FileUtils
import org.influxdb.{InfluxDB, InfluxDBFactory}

class StatsSender extends Actor with ScorexLogging {

  val influxDB: InfluxDB =
    InfluxDBFactory.connect(settings.influxDB.url, settings.influxDB.login, settings.influxDB.password )

  influxDB.setRetentionPolicy("autogen")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
    influxDB.write(8189, util.Arrays.asList(
      s"nodesStartTime value=${settings.network.nodeName}"
    ))
  }

  override def receive: Receive = {

    case SemanticallySuccessfulModifier(fb: EncryBlock) =>

      influxDB.write(8189, util.Arrays.asList(
        s"difficulty,nodeName=${settings.network.nodeName} value=${DifficultySerializer.decodeCompactBits(fb.header.nBits)}",
        s"height,nodeName=${settings.network.nodeName} value=${fb.header.height}",
        s"txsInBlock,nodeName=${settings.network.nodeName} value=${fb.payload.transactions.length}",
        s"stateWeight,nodeName=${settings.network.nodeName} value=${new File("encry/data/state/journal-1").length}",
        s"historyWeight,nodeName=${settings.network.nodeName} value=${FileUtils.sizeOfDirectory(new File("encry/data/history"))}",
        s"lastBlockSize,nodeName=${settings.network.nodeName} value=${fb.bytes.length}"
      )
      )

    case MiningEnd(blockHeader: EncryBlockHeader, workerNumber: Int) =>

      influxDB.write(8189, util.Arrays.asList(
        s"miningEnd,nodeName=${settings.network.nodeName},block=${Algos.encode(blockHeader.id)},height=${blockHeader.height},worker=$workerNumber value=${(timeProvider.time() - blockHeader.timestamp)/1000L}"
      ))
  }
}

object StatsSender {

  case class MiningEnd(blockHeader: EncryBlockHeader, workerNumber: Int)
}
