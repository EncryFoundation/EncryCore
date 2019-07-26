package encry.view.history

import encry.consensus.PowLinearController
import encry.modifiers.history.HeaderChain
import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height}
import org.encryfoundation.common.utils.constants.TestNetConstants

object HistoryUtils {

  def isNewHeader(header: Header, timeProvider: NetworkTimeProvider): Boolean =
    timeProvider.estimatedTime - header.timestamp <
      TestNetConstants.DesiredBlockInterval.toMillis * TestNetConstants.NewHeaderTimeMultiplier

}