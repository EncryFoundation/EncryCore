package encry.view

import supertagged.TaggedType

package object state {

  object Balance extends TaggedType[Long]

  type Balance = Balance.Type
}
