package encry.settings

trait Settings {
  val settings: EncryAppSettings = EncryAppSettings.read()
}