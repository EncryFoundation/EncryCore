package encry

import supertagged.TaggedType

package object crypto {

  object Address extends TaggedType[String]

  type Address = Address.Type

}
