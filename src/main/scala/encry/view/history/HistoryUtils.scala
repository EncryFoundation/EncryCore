package encry.view.history

import encry.EncryApp.settings
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.utils.constants.TestNetConstants

object HistoryUtils {

  val timeProvider = new NetworkTimeProvider(settings.ntp)

  def isNewHeader(header: Header): Boolean =
    timeProvider.estimatedTime - header.timestamp <
      TestNetConstants.DesiredBlockInterval.toMillis * TestNetConstants.NewHeaderTimeMultiplier

}