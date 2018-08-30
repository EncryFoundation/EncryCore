package encry

import supertagged.TaggedType

package object consensus {

  object Difficulty extends TaggedType[BigInt]

  type Difficulty = Difficulty.Type

  object NBits extends TaggedType[Long]

  type NBits = NBits.Type

  object Target extends TaggedType[BigInt]

  type Target = Target.Type

}
