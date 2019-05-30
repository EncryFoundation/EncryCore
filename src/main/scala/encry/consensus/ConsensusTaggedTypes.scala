package encry.consensus

import supertagged.TaggedType

object ConsensusTaggedTypes {

  object NBits extends TaggedType[Long]

  type NBits = NBits.Type

  object Target extends TaggedType[BigInt]

  type Target = Target.Type

}
