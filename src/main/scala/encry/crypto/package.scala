package encry

import supertagged.TaggedType

package object crypto {

  object Address extends TaggedType[Array[Byte]]

  type Address = Address.Type

}
