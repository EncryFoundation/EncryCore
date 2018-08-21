package encry.stats

import java.text.SimpleDateFormat
import java.util.{Calendar, Properties}
import akka.actor.Actor
import encry.settings.EncryAppSettings
import encry.utils.Logging
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}
import org.apache.kafka.common.serialization.StringSerializer

class KafkaActor extends Actor with Logging {

  import KafkaActor.KafkaMessage

  lazy val settings: EncryAppSettings = EncryAppSettings.read
  lazy val kafkaBrokers: String = settings.node.kafkaBrokers
  lazy val nodeName: String = settings.network.nodeName
  val topicName: String = "Logs"

  val formatter: SimpleDateFormat = new SimpleDateFormat("yyyy:mm:dd:HH:mm:ss")
  def getDateTimeNow: String = formatter.format(Calendar.getInstance().getTime)
  def assembleFullMessage(level: String, message: String): String = nodeName + " - " + getDateTimeNow + " - " +
    level + " - " + message

  val kafkaParams: Properties= new Properties
  kafkaParams.put("bootstrap.servers", kafkaBrokers)
  kafkaParams.put("key.serializer", classOf[StringSerializer])
  kafkaParams.put("value.serializer", classOf[StringSerializer])
  kafkaParams.put("group.id", "encry")

  val producer: KafkaProducer[String,String] = new KafkaProducer[String,String](kafkaParams)

  override def receive: Receive = {
    case KafkaMessage(level: String, message: String) =>
      producer.send(new ProducerRecord[String,String](topicName, java.util.UUID.randomUUID().toString,
        assembleFullMessage(level, message)))
  }

  override def preStart(): Unit = super.preStart()

  override def postStop(): Unit = super.postStop()

}

object KafkaActor {

  case class KafkaMessage(level: String, message: String)

}