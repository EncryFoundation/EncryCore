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
