package encry.stats

import java.util.concurrent.TimeUnit

import akka.actor.Actor
import encry.EncryApp.settings
import encry.consensus.DifficultySerializer
import encry.modifiers.history.block.EncryBlock
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.settings.Algos
import org.influxdb.InfluxDBFactory
import org.influxdb.dto.Point
import scorex.core.utils.ScorexLogging

class StatsSender extends Actor with ScorexLogging {

  val influxDB = InfluxDBFactory.connect( "http://172.16.10.55:8086", "admin", "EncryAdminNx89#GbLa7" )
  influxDB.setDatabase("testnet")
  influxDB.setRetentionPolicy("autogen")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {

    case SemanticallySuccessfulModifier(fb: EncryBlock) =>
      influxDB.write(Point.measurement("blockApply")
        .time(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
        .addField("name", settings.network.nodeName)
        .addField("ModifierId", Algos.encode(fb.id))
        .build())
      influxDB.write(Point.measurement("trxsInBlock")
        .time(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
        .addField("name", settings.network.nodeName)
        .addField("blockId", Algos.encode(fb.id))
        .addField("value", fb.payload.transactions.length)
        .build())
      influxDB.write(Point.measurement("blockMiner")
        .time(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
        .addField("name", settings.network.nodeName)
        .addField("blockId", Algos.encode(fb.id))
        .addField("value", Algos.encode(fb.header.accountPubKey.pubKeyBytes))
        .build())
      influxDB.write(Point.measurement("stateRoot")
        .time(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
        .addField("name", settings.network.nodeName)
        .addField("blockId", Algos.encode(fb.id))
        .addField("value", Algos.encode(fb.header.stateRoot))
        .build())
      influxDB.write(Point.measurement("blockMiningEnd")
        .time(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
        .addField("name", settings.network.nodeName)
        .addField("blockId", Algos.encode(fb.id))
        .addField("value", fb.header.timestamp)
        .build())
      influxDB.write(Point.measurement("blockHeight")
        .time(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
        .addField("name", settings.network.nodeName)
        .addField("blockId", Algos.encode(fb.id))
        .addField("value", fb.header.height)
        .build())
      influxDB.write(Point.measurement("blockDifficulty")
        .time(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
        .addField("name", settings.network.nodeName)
        .addField("blockId", Algos.encode(fb.id))
        .addField("value", DifficultySerializer.decodeCompactBits(fb.header.nBits))
        .build())
  }
}
