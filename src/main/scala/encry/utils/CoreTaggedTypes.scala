package encry.utils

import supertagged.TaggedType

object CoreTaggedTypes {

  object VersionTag extends TaggedType[Array[Byte]]

  type VersionTag = VersionTag.Type

}