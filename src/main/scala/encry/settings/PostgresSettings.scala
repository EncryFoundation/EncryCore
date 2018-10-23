package encry.settings

case class PostgresSettings(host: String,
                            user: String,
                            password: String,
                            maxPoolSize: Int,
                            enableSave: Boolean = false,
                            enableRestore: Boolean = false,
                            restoreBatchSize: Option[Int] = None,
                            writingGap: Option[Int] = None)