package encry.stats


import java.util

import akka.actor.Actor
import com.typesafe.config.{Config, ConfigFactory}
import encry.EncryApp.settings
import encry.consensus.DifficultySerializer
import encry.modifiers.history.block.EncryBlock
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scorex.core.utils.ScorexLogging

class StatsSender extends Actor with ScorexLogging {

  val influxDBConfig: Config = ConfigFactory.load("influxDB")

  val influxDB: InfluxDB =
    InfluxDBFactory.connect(influxDBConfig.getString("urlDB"), influxDBConfig.getString("loginDB"), influxDBConfig.getString("passwordDB") )

  influxDB.setRetentionPolicy("autogen")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {

    case SemanticallySuccessfulModifier(fb: EncryBlock) =>

      influxDB.write(8189, util.Arrays.asList(
        s"difficulty,nodeName=${settings.network.nodeName} value=${DifficultySerializer.decodeCompactBits(fb.header.nBits)}",
        s"height,nodeName=${settings.network.nodeName} value=${fb.header.height}",
        s"height,nodeName=${settings.network.nodeName} value=${fb.payload.transactions.length}" )
      )

  }
}
