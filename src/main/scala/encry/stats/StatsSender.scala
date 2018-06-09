package encry.stats


import java.util

import akka.actor.Actor
import encry.EncryApp.settings
import encry.consensus.DifficultySerializer
import encry.modifiers.history.block.EncryBlock
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scorex.core.utils.ScorexLogging

class StatsSender extends Actor with ScorexLogging {

  val influxDB: InfluxDB =
    InfluxDBFactory.connect(settings.influxDB.url, settings.influxDB.login, settings.influxDB.password )

  influxDB.setRetentionPolicy("autogen")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {

    case SemanticallySuccessfulModifier(fb: EncryBlock) =>

      influxDB.write(8189, util.Arrays.asList(
        s"difficulty,nodeName=${settings.network.nodeName} value=${DifficultySerializer.decodeCompactBits(fb.header.nBits)}",
        s"height,nodeName=${settings.network.nodeName} value=${fb.header.height}",
        s"txsInBlock,nodeName=${settings.network.nodeName} value=${fb.payload.transactions.length}" )
      )

  }
}
