package encry.settings

trait AdditionalTestSettings {
  //settings with known peers
  lazy val additionalTestSettings: EncryAppSettings = EncryAppSettings.loadConfig("AdditionalTestSettings.conf")
}
