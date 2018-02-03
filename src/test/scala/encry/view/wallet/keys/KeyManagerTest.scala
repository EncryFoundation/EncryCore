package encry.view.wallet.keys

import encry.settings.{Algos, EncryAppSettings}
import org.scalatest.FunSuite

class KeyManagerTest extends FunSuite {

  test("get keys"){

    lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(Some("encry"))

    val keyManager = KeyManager.readOrGenerate(encrySettings, Option(Algos.hash("Password")))
  }
}
