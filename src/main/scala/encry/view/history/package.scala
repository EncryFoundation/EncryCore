package encry.view

import supertagged.TaggedType

package object history {

  object Height extends TaggedType[Int]

  type Height = Height.Type
}
