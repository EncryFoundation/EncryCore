package encry.settings

trait TestSettings {
  lazy val settings: EncryAppSettings = EncryAppSettings.loadConfig("MainTestSettings.conf")
}
