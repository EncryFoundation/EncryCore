package encry.view.history

import encry.utils.CoreTaggedTypes.ModifierId
import encry.consensus.SyncInfo
import encry.EncryApp.settings
import encry.modifiers.history.block.header.Header
import encry.consensus.History.ModifierIds
import encry.modifiers.NodeViewModifier
import encry.network.message.SyncInfoMessageSpec
import org.encryfoundation.common.serialization.Serializer
import scala.util.Try

case class EncrySyncInfo(lastHeaderIds: Seq[ModifierId]) extends SyncInfo {

  override type M = EncrySyncInfo

  override def startingPoints: ModifierIds = lastHeaderIds.map(id => Header.modifierTypeId -> id)

  override lazy val serializer: Serializer[M] = EncrySyncInfoSerializer
}

object EncrySyncInfoSerializer extends Serializer[EncrySyncInfo] {

  override def toBytes(obj: EncrySyncInfo): Array[Byte] = concatFixLengthBytes(obj.lastHeaderIds)

  def concatFixLengthBytes(seq: Traversable[Array[Byte]]): Array[Byte] = seq.headOption match {
    case None       => Array[Byte]()
    case Some(head) => concatFixLengthBytes(seq, head.length)
  }

  def concatFixLengthBytes(seq: Traversable[Array[Byte]], length: Int): Array[Byte] = {
    val result: Array[Byte] = new Array[Byte](seq.toSeq.length * length)
    var index = 0
    seq.foreach { s =>
      Array.copy(s, 0, result, index, length)
      index += length
    }
    result
  }

  override def parseBytes(bytes: Array[Byte]): Try[EncrySyncInfo] = Try {
    require(bytes.length <= settings.network.syncPacketLength * NodeViewModifier.ModifierIdSize + 1)

    val ids = ModifierId @@ bytes.grouped(NodeViewModifier.ModifierIdSize).toSeq

    EncrySyncInfo(ids)
  }
}

object EncrySyncInfoMessageSpec extends SyncInfoMessageSpec[EncrySyncInfo](EncrySyncInfoSerializer.parseBytes)
