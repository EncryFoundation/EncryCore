package encry.modifiers.serialization

trait BytesSerializable extends Serializable {

  type M >: this.type <: BytesSerializable

  lazy val bytes: Array[Byte] = serializer.toBytes(this)

  def serializer: Serializer[M]
}
