import encry.network.message.BasicMsgDataTypes.InvData
import encry.settings.Algos
import supertagged.TaggedType

package object encry {

  object ModifierTypeId extends TaggedType[Byte]

  object ModifierId extends TaggedType[Array[Byte]]

  object VersionTag extends TaggedType[Array[Byte]]

  object Address extends TaggedType[String]

  type Address = Address.Type

  type ModifierTypeId = ModifierTypeId.Type

  type ModifierId = ModifierId.Type

  type VersionTag = VersionTag.Type

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)]): String = (ids.headOption, ids.lastOption) match {
    case (Some(f), Some(l)) if f._2 sameElements l._2 => s"[(${f._1},${Algos.encode(f._2)})]"
    case (Some(f), Some(l)) => s"[(${f._1},${Algos.encode(f._2)})..(${l._1},${Algos.encode(l._2)})]"
    case _ => "[]"
  }

  def idsToString(modifierType: ModifierTypeId, ids: Seq[ModifierId]): String = {
    idsToString(ids.map(id => (modifierType, id)))
  }

  def idsToString(invData: InvData): String = idsToString(invData._1, invData._2)

}