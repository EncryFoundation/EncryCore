package encry

import supertagged.TaggedType

package object account {

  object Address extends TaggedType[String]

  type Address = Address.Type

}
