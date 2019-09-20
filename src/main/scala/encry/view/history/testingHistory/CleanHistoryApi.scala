package encry.view.history.testingHistory

import encry.view.history.History
import org.encryfoundation.common.modifiers.PersistentModifier

trait CleanHistoryApi {

  /**
    * @param modifier
    * @return
    */
  def append(modifier: PersistentModifier): Either[Throwable, History]

  /**
    * @param modifier
    * @return
    */
  def markModifierAsValid(modifier: PersistentModifier): Either[Throwable, History]

  /**
    * @param modifier
    * @return
    */
  def markModifierAsInValid(modifier: PersistentModifier): Either[Throwable, History]

}