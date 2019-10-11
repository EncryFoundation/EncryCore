package encry.view.fastSync

import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.view.fastSync.SnapshotHolder.SnapshotManifest
import cats.syntax.option._

final case class GivingChunksProcessor(peer: Option[ConnectedPeer],
                                       manifest: Option[SnapshotManifest],
                                       lastsIds: List[Array[Byte]]) {

  def updateLastsIds(forRemove: Array[Byte], cp: ConnectedPeer): GivingChunksProcessor =
      this.copy(peer = cp.some, lastsIds = lastsIds.filterNot(_.sameElements(forRemove)))
}

object GivingChunksProcessor {
  def empty: GivingChunksProcessor = GivingChunksProcessor(none, none, List.empty)
}
