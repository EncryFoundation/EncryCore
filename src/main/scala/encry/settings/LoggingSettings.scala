package encry.settings

case class LoggingSettings(sendToInflux: Boolean,
                           sendToLocalFile: Boolean,
                           sendToKafka: Boolean,
                           enableLogging: Boolean)
