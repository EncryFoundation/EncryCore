package encry.stats

import java.util.concurrent.TimeUnit

import akka.actor.Actor
import com.typesafe.config.{Config, ConfigFactory}
import encry.modifiers.history.block.EncryBlock
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.settings.Algos
import org.influxdb.dto.Point
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scorex.core.utils.ScorexLogging

class StatsSender extends Actor with ScorexLogging {

  val influxDBConfig: Config = ConfigFactory.load("influxDB")

  val influxDB: InfluxDB =
    InfluxDBFactory.connect("http://172.16.10.55:8086", influxDBConfig.getString("loginDB"), influxDBConfig.getString("passwordDB") )

  influxDB.setRetentionPolicy("autogen")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {

    case SemanticallySuccessfulModifier(fb: EncryBlock) =>

      influxDB.write(8189, Point.measurement("blockApply")
        .time(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
        .addField("ModifierId", Algos.encode(fb.id))
        .addField("value", fb.payload.transactions.length)
        .build().lineProtocol())
  }
}
