package encry.modifiers.state.box.serializers

import scorex.core.serialization.Serializer

trait BoxCompanionSerializer[M] extends Serializer[M] {
  val Length: Int
}
