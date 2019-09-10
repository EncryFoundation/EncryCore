package encry.settings

trait MainTestSettings {
  //settings with all peers
  lazy val mainTestSettings: EncryAppSettings = EncryAppSettings.loadConfig("MainTestSettings.conf")
}
