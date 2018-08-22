package encry.settings

case class KafkaSettings(sendToKafka: Boolean,
                         topicName: String,
                         groupId: String,
                         kafkaBrokers: String)
