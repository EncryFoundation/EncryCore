package encry

import supertagged.TaggedType

package object account {

  object Address extends TaggedType[String]

  type Address = Address.Type

  object Balance extends TaggedType[Long]

  type Balance = Balance.Type
}
