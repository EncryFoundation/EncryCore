package encry.modifiers.mempool

import encry.modifiers.InstanceFactory
import io.circe
import org.scalatest.FunSuite

import scala.util.Try

class EncryTransactionSerializerTest extends FunSuite with InstanceFactory {

  test("toBytes & parseBytes (Transfer)") {

    val tx = paymentTransactionDynamic

    val txSerialized = tx.bytes

    val txDeserialized = EncryTransactionSerializer.parseBytes(txSerialized)

    assert(txDeserialized.isSuccess, "Deserialization failed.")

    assert(tx.id sameElements txDeserialized.get.id, "Id mismatch.")
  }

  test("toBytes & parseBytes (Coinbase)") {

    val tx = coinbaseTransaction

    val txSerialized = tx.bytes

    val txDeserialized = EncryTransactionSerializer.parseBytes(txSerialized)

    assert(txDeserialized.isSuccess, "Deserialization failed.")

    assert(tx.id sameElements txDeserialized.get.id, "Id mismatch.")
  }

  test("Transaction with script") {

    import EncryTransaction._
    import io.circe.parser._

    val txJson: String =
      """
        |{
        |  "directives" : [
        |    {
        |      "typeId" : 3,
        |      "contract" : "2GyFdmWbQSUfKvDjmSYRuhv2w3SHEi3ZkJiVNV1WTqPviBdqVYwX676PyjsQbZJL9N98maYDxb6gHfybggbsWpU2Xajh7cNRiZvCWHqBHu9u3UP4HFpjiDEtz2uk8itvgyWYviyyLLyw9Mk9nsxFdnAcYW7jncqwDdd3pSFN9wPehJ8JTwoDkfRhhiDwaxy562sX8PbvVdqxWjQbe6BKT1xt696YzZWgMPTQwNdTmwF1pAe2Rss3veMcskm31AsPd7tqivNUrvgp67Naz9JsTgzaqvrjoqGw",
        |      "amount" : 1000,
        |      "tokenId" : null
        |    },
        |    {
        |      "typeId" : 1,
        |      "address" : "4GN4tJYvyKkP4XHZPLatmaDwYQDaSsasUUVxx9XSLGpVwP9pN2",
        |      "amount" : 2317100,
        |      "tokenId" : null
        |    }
        |  ],
        |  "timestamp" : 1529584879761,
        |  "defaultProofOpt" : {
        |    "serializedValue" : "oCNHxErcGTyoaoWSCAGBuJuRANsBgLqrmtGTzRj58zmisBZSbxRQLmiittc9oLn8EXxihggYs8rqx9u889Lj1LEvpYRF4T",
        |    "tag" : null
        |  },
        |  "id" : "6RVZuxpihf1SncJEVt6pWfaEeGbnx5B49u3pjF5bCHGq",
        |  "inputs" : [
        |    {
        |      "boxId" : "5nveWAFkSNJQdiM6bNySUUd8F5R6h3jyx648398qrMr",
        |      "proofs" : [
        |      ]
        |    }
        |  ],
        |  "fee" : 100
        |}
      """.stripMargin

    val tx: Either[circe.Error, EncryTransaction] = decode[EncryTransaction](txJson)

    val txSer: Array[Byte] = tx.toTry.get.bytes

    val txDes: Try[EncryTransaction] = EncryTransactionSerializer.parseBytes(txSer)

    assert(txDes.isSuccess)
  }
}
