package encry.view.actors

import akka.actor.{Actor, ActorRef}
import com.google.common.primitives.Ints
import com.typesafe.scalalogging.StrictLogging
import encry.view.actors.SnapshotHolder._
import encry.view.state.UtxoState
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ModifierId
import scorex.crypto.hash.Digest32

class SnapshotHolder extends Actor with StrictLogging {

  var lastSnapshot: Snapshot = Snapshot.empty
  var lastManifest: SnapshotManifest = SnapshotManifest.empty

  override def preStart(): Unit = super.preStart()
  // networkController ! RegisterMessagesHandler(smth, self)

  override def receive: Receive = {
    case UpdateSnapshot(block, state) =>
      logger.info(s"Updating current snapshot info.")
      val newManifest: SnapshotManifest = SnapshotManifest(block.header.height, block.id, state.tree.rootHash)
      //todo implement later
      val newSnapshot: Snapshot = Snapshot.empty
      lastManifest = newManifest
      lastSnapshot = newSnapshot
    case RequestManifest(remote) =>
      logger.info(s"Got request from $remote.")
      remote ! ResponseManifest(lastManifest)
    case RequestSnapshot(remote) =>
      logger.info(s"Got request for snapshot from $remote.")
      remote ! ResponseSnapshot(lastSnapshot)
  }
}

object SnapshotHolder {

  final case class UpdateSnapshot(bestBlock: Block, state: UtxoState)

  final case class RequestManifest(remote: ActorRef) extends AnyVal

  final case class ResponseManifest(manifest: SnapshotManifest) extends AnyVal

  final case class RequestSnapshot(remote: ActorRef) extends AnyVal

  final case class ResponseSnapshot(snapshot: Snapshot) extends AnyVal

  final case class SnapshotManifest(snapshotHeight: Int,
                                    bestBlockId: ModifierId,
                                    rootHash: Array[Byte]) {
    val ManifestId: Digest32 = Algos.hash(Ints.toByteArray(snapshotHeight) ++ bestBlockId ++ rootHash)
    val snapshotId = ???
  }
  object SnapshotManifest {
    def empty: SnapshotManifest = SnapshotManifest(-1, ModifierId @@ Array.emptyByteArray, Array.emptyByteArray)
  }

  //todo add hash of concatenated chunks into network message
  final case class Snapshot(manifestIf: Digest32, chunks: List[Array[Byte]]) {
    val SnapshotId = ???
  }
  object Snapshot {
    def empty: Snapshot = Snapshot(Digest32 @@ Array.emptyByteArray, List.empty[Array[Byte]])
  }
}