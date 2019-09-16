package encry.settings

trait TestNetSettings {
  //settings with all peers
  lazy val settings: EncryAppSettings = EncryAppSettings.loadConfig("TestNetSettings.conf")
}
