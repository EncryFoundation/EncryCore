package encry.view.history.tmp

import encry.utils.NetworkTimeProvider
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

import scala.annotation.tailrec

trait HistoryPrivateApi extends HistoryPublicAPI {

  protected[history] val timeProvider: NetworkTimeProvider

  protected[history] final def continuationHeaderChains(
    header: Header,
    filterCond: Header => Boolean
  ): List[List[Header]] = {
    @tailrec def loop(
      currentHeight: Int,
      acc: List[List[Header]]
    ): List[List[Header]] = {
      val nextHeightHeaders: List[Header] = headerIdsAtHeight(currentHeight + 1).view
        .flatMap(getHeaderById)
        .filter(filterCond)
        .toList
      if (nextHeightHeaders.isEmpty) acc.map(_.reverse)
      else {
        val updatedChains: List[List[Header]] = nextHeightHeaders.flatMap { h: Header =>
          acc.find((chain: List[Header]) => chain.nonEmpty && (h.parentId sameElements chain.head.id)).map(h +: _)
        }
        val nonUpdatedChains: List[List[Header]] =
          acc.filter((chain: List[Header]) => !nextHeightHeaders.exists(_.parentId sameElements chain.head.id))

        loop(currentHeight + 1, updatedChains ++ nonUpdatedChains)
      }
    }

    loop(header.height, List(List(header)))
  }

  @tailrec protected[history] final def loopHeightDown(
    height: Int,
    p: ModifierId => Boolean
  ): Option[Header] =
    headerIdsAtHeight(height)
      .find(p)
      .flatMap(getHeaderById) match {
      case h @ Some(_)                                       => h
      case None if height > settings.constants.GenesisHeight => loopHeightDown(height - 1, p)
      case n @ None                                          => n
    }

}
