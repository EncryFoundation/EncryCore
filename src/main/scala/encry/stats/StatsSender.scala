package encry.stats

import java.io.File
import java.util

import akka.actor.Actor
import encry.EncryApp.{settings, timeProvider}
import encry.consensus.DifficultySerializer
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.stats.StatsSender.MiningEnd
import encry.utils.ScorexLogging
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scorex.core.ModifierId

class StatsSender extends Actor with ScorexLogging {

  val influxDB: InfluxDB =
    InfluxDBFactory.connect(settings.influxDB.url, settings.influxDB.login, settings.influxDB.password )

  influxDB.setRetentionPolicy("autogen")

  var minedBlocks: Map[ModifierId, Long] = Map.empty[ModifierId, Long]

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
    influxDB.write(8189, util.Arrays.asList(
      s"nodesStartTime value=${settings.network.nodeName}"
    ))
  }

  override def receive: Receive = {

    case SemanticallySuccessfulModifier(fb: EncryBlock) =>

      influxDB.write(8189, util.Arrays.asList(
        minedBlocks.get(fb.header.id).map( value =>
          s"miningEnd,nodeName=${settings.network.nodeName} value=$value"
        ).getOrElse(""),
        s"difficulty,nodeName=${settings.network.nodeName} value=${DifficultySerializer.decodeCompactBits(fb.header.nBits)}",
        s"height,nodeName=${settings.network.nodeName} value=${fb.header.height}",
        s"txsInBlock,nodeName=${settings.network.nodeName} value=${fb.payload.transactions.length}",
        s"lastBlockSize,nodeName=${settings.network.nodeName} value=${fb.bytes.length}",
        s"stateSize,nodeName=${settings.network.nodeName} value=${new File("encry/data/state/").listFiles.foldLeft(0L)(_ + _.length)/1024}",
        s"historySize,nodeName=${settings.network.nodeName} value=${new File("encry/data/history/").listFiles.foldLeft(0L)(_ + _.length)/1024}"
        )
      )

      minedBlocks = Map.empty[ModifierId, Long]

    case MiningEnd(blockHeader: EncryBlockHeader, workerNumber: Int) =>
      minedBlocks += (blockHeader.id -> (timeProvider.time() - blockHeader.timestamp)/1000L)
  }
}

object StatsSender {

  case class MiningEnd(blockHeader: EncryBlockHeader, workerNumber: Int)
}
