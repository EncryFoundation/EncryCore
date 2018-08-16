package encry.stats

import java.util.Properties
import org.apache.kafka.clients.producer.{KafkaProducer, ProducerRecord}
import org.apache.kafka.common.serialization.StringSerializer

object KafkaAgent {
  val kafkaBrokers = "localhost:9092"
  val topicName = "Logs"

  val kafkaParams = new Properties()

  kafkaParams.put("bootstrap.servers", kafkaBrokers)
  kafkaParams.put( "key.serializer", classOf[StringSerializer])
  kafkaParams.put("value.serializer", classOf[StringSerializer])
  kafkaParams.put("group.id", "george")

  val producer = new KafkaProducer[String,String](kafkaParams)

  def sendEvent(message: String) : Unit = {
    val key = java.util.UUID.randomUUID().toString
    producer.send(new ProducerRecord[String, String](topicName, key, message))
  }
}
