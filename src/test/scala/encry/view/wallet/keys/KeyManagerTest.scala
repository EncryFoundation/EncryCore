package encry.view.wallet.keys

import com.google.common.primitives.Ints
import encry.settings.{Algos, EncryAppSettings}
import org.scalatest.FunSuite
import scorex.crypto.encode.Base58

class KeyManagerTest extends FunSuite {

  test("get keys"){

    lazy val encrySettings: EncryAppSettings = EncryAppSettings.read(Option(""))

    val keyManager = KeyManager.readOrGenerate(encrySettings, Option(Algos.hash("Password")))
  }
}
