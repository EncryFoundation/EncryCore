package encry.modifiers.state

import encry.modifiers.state.box._

import scala.util.{Failure, Try}

object StateModifierDeserializer {

  def parseBytes(bytes: Array[Byte], typeId: Byte): Try[EncryBaseBox] = typeId match {
    case OpenBox.`typeId` => OpenBoxSerializer.parseBytes(bytes)
    case AssetBox.`typeId` => AssetBoxSerializer.parseBytes(bytes)
    case PubKeyInfoBox.`typeId` => PubKeyInfoBoxSerializer.parseBytes(bytes)
    case t => Failure(new Error(s"Got unknown typeId: $t"))
  }
}
