package encry.settings

case class PostgresSettings(host: String,
                            user: String,
                            password: String,
                            maxPoolSize: Int,
                            enableSave: Boolean = false,
                            writingGap: Option[Int] = None)
