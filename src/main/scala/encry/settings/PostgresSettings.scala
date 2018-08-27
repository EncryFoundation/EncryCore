package encry.settings

case class PostgresSettings(host: String,
                            user: String,
                            password: String,
                            enabledSave: Boolean = false,
                            enableRestore: Boolean = false,
                            restoreBatchSize: Option[Int] = None)
