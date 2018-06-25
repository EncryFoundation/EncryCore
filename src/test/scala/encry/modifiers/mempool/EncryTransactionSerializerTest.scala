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
        |      "address" : "3JZkxjCJXsg1Kf6cZMPg5cVkN4Dnn1pHB8jhzfKemCaAMbwWLN",
        |      "amount" : 199000,
        |      "tokenId" : "null"
        |    },
        |    {
        |      "typeId" : 1,
        |      "address" : "3JZkxjCJXsg1Kf6cZMPg5cVkN4Dnn1pHB8jhzfKemCaAMbwWLN",
        |      "amount" : 1796700,
        |      "tokenId" : "null"
        |    }
        |  ],
        |  "timestamp" : 1529938861189,
        |  "defaultProofOpt" : {
        |    "serializedValue" : "oCNHxEvarQuZk9tdiwNmD9igNVri6Jdm7HWi6t6UwXwrZ69iZL7T9E8gMuzcVyggGEmSjdGogircnYwGn5HKhstSZMbUzj",
        |    "tag" : null
        |  },
        |  "id" : "F6bD6Rrkw2G7Z5F5AzUAsgF8aeEULQbWAQjhTSJh6sLs",
        |  "inputs" : [
        |    {
        |      "boxId" : "8DYAn2KiRDFtZcUyqYrnjTurgNgK216heLLUGMXL51b",
        |      "contract" : "1114o4rYAeuF7S9dD9MNpoXJ3UkUUwk2JC85YpZ1BoDKx1HFypN8o2F25QGPTm4E8oAMKEUBwTUg8pP1n3sfET1HvxR5LVRyx8ANKBxrnWmJ8B4i4hvzYo5KkSdMTm55yiB9XZqM3o1m6DtQ6dSgP9LKbKhDiFF49otkj7RT79qWMMyairgRGDwtbTq8oNTDvvpZkZMZJ36YcXg3Pdxo7KAaicM5BWU2w5ZZoztga3zPfn9aAC4J",
        |      "proofs" : [
        |      ]
        |    },
        |    {
        |      "boxId" : "8DYAn2KiRDFtZcUyqYrnjTurgNgK216heLLUGMXL51b",
        |      "contract" : "1114o4rYAeuF7S9dD9MNpoXJ3UkUUwk2JC85YpZ1BoDKx1HFypN8o2F25QGPTm4E8oAMKEUBwTUg8pP1n3sfET1HvxR5LVRyx8ANKBxrnWmJ8B4i4hvzYo5KkSdMTm55yiB9XZqM3o1m6DtQ6dSgP9LKbKhDiFF49otkj7RT79qWMMyairgRGDwtbTq8oNTDvvpZkZMZJ36YcXg3Pdxo7KAaicM5BWU2w5ZZoztga3zPfn9aAC4J",
        |      "proofs" : [
        |      ]
        |    }
        |  ],
        |  "fee" : 4300
        |}
      """.stripMargin

    val tx: Either[circe.Error, EncryTransaction] = decode[EncryTransaction](txJson)

    val txSer: Array[Byte] = tx.toTry.get.bytes

    val txDes: Try[EncryTransaction] = EncryTransactionSerializer.parseBytes(txSer)

    assert(txDes.isSuccess)
  }
}
