package encry.settings

case class PostgresSettings(host: String,
                            user: String,
                            password: String,
                            maxPoolSize: Int,
                            writingGap: Option[Int] = None)
