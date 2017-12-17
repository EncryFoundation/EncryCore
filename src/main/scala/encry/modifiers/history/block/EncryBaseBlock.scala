package encry.modifiers.history.block

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBaseBlockPayload
import scorex.core.EphemerealNodeViewModifier
import scorex.core.utils.ScorexLogging

import scala.util.Try

trait EncryBaseBlock extends EncryPersistentModifier with ScorexLogging {

  val header: EncryBlockHeader

  val payload: EncryBaseBlockPayload

  val toSeq: Seq[EncryPersistentModifier]

  def semanticValidity: Try[Unit]

  def transactions: Seq[EphemerealNodeViewModifier]
}

