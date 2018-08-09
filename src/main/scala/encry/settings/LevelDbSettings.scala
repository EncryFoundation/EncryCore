package encry.settings

case class LevelDbSettings(enable: Boolean,
                           recoverMode: Boolean,
                           batchSize: Int)
