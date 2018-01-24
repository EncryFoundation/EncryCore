package encry.modifiers.mempool

import org.scalatest.FunSuite
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.authds.ADKey
import scorex.crypto.signatures.{PublicKey, Signature}
import scorex.utils.Random


class CoinbaseTransactionSerializerTest extends FunSuite {

  test("test ParseBytes & toBytes") {

    val coinbase = InstanceFactory.coinbaseTransaction

    val coinbaseSerialized = CoinbaseTransactionSerializer.toBytes(coinbase)

    val coinbaseDeserialized = CoinbaseTransactionSerializer.parseBytes(coinbaseSerialized)

    assert(coinbaseDeserialized.isSuccess, "Deserialization failed")

    assert(coinbase.txHash sameElements coinbaseDeserialized.get.txHash, "Deserialization result mismatch")

    assert(coinbase.useBoxes.zip(coinbaseDeserialized.get.useBoxes).forall(i => i._1 sameElements i._2),
      "Deserialization result mismatch. ADKeys were parsed incorrectly.")
  }

}
