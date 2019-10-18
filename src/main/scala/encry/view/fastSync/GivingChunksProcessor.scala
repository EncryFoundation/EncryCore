package encry.view.fastSync

import java.net.InetSocketAddress

import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.view.fastSync.SnapshotHolder.SnapshotManifest
import cats.syntax.option._
import io.iohk.iodb.ByteArrayWrapper

final case class GivingChunksProcessor(peer: Option[ConnectedPeer],
                                       manifest: Option[SnapshotManifest],
                                       notYetRequestedChunksIds: Set[ByteArrayWrapper]) {

  def processChunk(chunkId: Array[Byte], manifestId: Array[Byte]): Boolean =
    notYetRequestedChunksIds.contains(ByteArrayWrapper(chunkId)) && manifest.exists(
      _.ManifestId.sameElements(manifestId)
    )

  def canPeerBeProcessed(remote: InetSocketAddress): Boolean =
    peer.isEmpty || peer.exists(_.socketAddress == remote)

  def updateLastsIds(forRemove: Array[Byte], cp: ConnectedPeer): GivingChunksProcessor =
    this.copy(peer = cp.some, notYetRequestedChunksIds = notYetRequestedChunksIds - ByteArrayWrapper(forRemove))
}

object GivingChunksProcessor {
  def empty: GivingChunksProcessor = GivingChunksProcessor(none, none, Set.empty)
}
