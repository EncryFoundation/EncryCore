package encry.view.history

import encry.modifiers.history.block.header.EncryBlockHeader
import scorex.core.{ModifierId, NodeViewModifier}
import scorex.core.consensus.History.ModifierIds
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.Serializer

import scala.util.Try

case class EncrySyncInfo(lastHeaderIds: Seq[ModifierId]) extends SyncInfo {

  override type M = EncrySyncInfo

  override def startingPoints: ModifierIds = lastHeaderIds.map(b => EncryBlockHeader.modifierTypeId -> b)

  override lazy val serializer: Serializer[M] = EncrySyncInfoSerializer
}

object EncrySyncInfo {
  val MaxBlockIds = 1000
}

object EncrySyncInfoSerializer extends Serializer[EncrySyncInfo] {

  override def toBytes(obj: EncrySyncInfo): Array[Byte] = {
    scorex.core.utils.concatFixLengthBytes(obj.lastHeaderIds)
  }

  override def parseBytes(bytes: Array[Byte]): Try[EncrySyncInfo] = Try {
    require(bytes.length <= EncrySyncInfo.MaxBlockIds * NodeViewModifier.ModifierIdSize + 1)

    val ids = ModifierId @@ bytes.grouped(NodeViewModifier.ModifierIdSize).toSeq

    EncrySyncInfo(ids)
  }
}

object EncrySyncInfoMessageSpec extends SyncInfoMessageSpec[EncrySyncInfo](EncrySyncInfoSerializer.parseBytes)