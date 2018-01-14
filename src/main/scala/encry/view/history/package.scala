package encry.view

import supertagged.TaggedType

package object history {

  object Height extends TaggedType[Long]

  type Height = Height.Type
}
