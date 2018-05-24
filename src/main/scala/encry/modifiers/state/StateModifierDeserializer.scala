package encry.modifiers.state

import encry.modifiers.state.box._

import scala.util.{Failure, Try}

object StateModifierDeserializer {

  def parseBytes(bytes: Array[Byte], typeId: Byte): Try[EncryBaseBox] = typeId match {
    case CoinbaseBox.`TypeId` => CoinbaseBoxSerializer.parseBytes(bytes)
    case AssetBox.`TypeId` => AssetBoxSerializer.parseBytes(bytes)
    case AssetIssuingBox.`TypeId` => AssetIssuingBoxSerializer.parseBytes(bytes)
    case AssetCreationBox.`TypeId` => AssetCreationBoxSerializer.parseBytes(bytes)
    case DataBox.`TypeId` => DataBoxSerializer.parseBytes(bytes)
    case t => Failure(new Exception(s"Got unknown typeId: $t"))
  }
}
