package encry.settings

case class PostgresSettings(host: String,
                            user: String,
                            password: String,
                            enableSave: Boolean = false,
                            enableRestore: Boolean = false,
                            restoreBatchSize: Option[Int] = None)
