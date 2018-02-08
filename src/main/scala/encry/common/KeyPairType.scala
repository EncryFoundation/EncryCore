package encry.common

sealed trait KeyPairType {

  def typeId: Byte

  def name: String
}

object KeyPairType {

  def pairTypeById(typeId: Byte): KeyPairType = typeId match {
    case Pair25519.typeId => Pair25519
  }

  def pairTypeByName(name: String): KeyPairType = name match {
    case Pair25519.name => Pair25519
  }
}

case object Pair25519 extends KeyPairType {

  val typeId: Byte = 99.toByte

  val name: String = "Pair25519"
}
