package encry.settings

trait TestNetSettings {
  val testNetSettings: EncryAppSettings = EncryAppSettings.loadConfig("TestNetSettings.conf")
}
