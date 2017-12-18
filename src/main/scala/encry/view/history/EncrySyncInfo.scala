package encry.view.history

import encry.modifiers.history.block.header.EncryBlockHeader
import scorex.core.ModifierId
import scorex.core.consensus.History.ModifierIds
import scorex.core.consensus.SyncInfo
import scorex.core.serialization.Serializer

import scala.util.Try

class EncrySyncInfo(override val answer: Boolean,
                    lastHeaderIds: Seq[ModifierId]) extends SyncInfo {

  override type M = EncrySyncInfo

  override def startingPoints: ModifierIds = lastHeaderIds.map(b => EncryBlockHeader.modifierTypeId -> b)

  override lazy val serializer: Serializer[M] = EncrySyncInfoSerializer
}

object EncrySyncInfo {
  val MaxBlockIds = 1000
}

object EncrySyncInfoSerializer extends Serializer[EncrySyncInfo] {

  override def toBytes(obj: EncrySyncInfo): Array[Byte] = ???

  override def parseBytes(bytes: Array[Byte]): Try[EncrySyncInfo] = ???
}
