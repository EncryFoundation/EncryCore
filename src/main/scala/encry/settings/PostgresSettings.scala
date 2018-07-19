package encry.settings

case class PostgresSettings(host: String,
                            user: String,
                            password: String,
                            enabled: Boolean = false)
