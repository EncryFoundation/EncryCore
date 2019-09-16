package encry.settings

trait TestNetSettings {
  lazy val testNetSettings: EncryAppSettings = EncryAppSettings.loadConfig("TestNetSettings.conf")
}
