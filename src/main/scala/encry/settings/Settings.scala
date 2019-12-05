package encry.settings

trait Settings {
  lazy val settings: EncryAppSettings = EncryAppSettings.read()
}