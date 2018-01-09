package encry.modifiers.state.box.serializers

import scorex.core.serialization.Serializer

trait SizedCompanionSerializer[M] extends Serializer[M] {
  val Size: Int
}
