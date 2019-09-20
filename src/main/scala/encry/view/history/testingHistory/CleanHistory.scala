package encry.view.history.testingHistory

import encry.view.history.History
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}

case class CleanHistory() extends CleanHistoryModifiersProcessor {

  def append(modifier: PersistentModifier): Either[Throwable, History] = {

  }

   def markModifierAsValid(modifier: PersistentModifier): Either[Throwable, History] = ???

  def markModifierAsInValid(modifier: PersistentModifier): Either[Throwable, History] = ???
}

object CleanHistory {

  final case class HistoryProcessingInfo(blockDownloadingProcessor: CleanBlockDownloadingProcessor,
                                         modifiersToDownload: Option[(ModifierTypeId, ModifierId)],
                                         modifiersToApply: Option[PersistentModifier])
}