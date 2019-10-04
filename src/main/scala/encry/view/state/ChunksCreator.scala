package encry.view.state

import NodeMsg.NodeProtoMsg.NodeTypes
import akka.util.ByteString
import encry.view.actors.SnapshotHolder.{Snapshot, SnapshotManifest}
import encry.view.state.avlTree.utils.implicits.Serializer
import encry.view.state.avlTree.utils.implicits.Instances._
import encry.view.state.avlTree.{EmptyNode, InternalNode, LeafNode, ShadowNode}
import scorex.crypto.hash.Digest32

object ChunksCreator {

  final case class StateChunk(manifestId: Digest32, serializedBytes: Array[Byte])

 def computeChunks(state: UtxoState, manifest: SnapshotManifest): Unit = {
   state.tree.rootNode match {
     case EmptyNode(key, value, height, balance) =>
     case n@InternalNode(key, value, height, balance, leftChild, rightChild, hash) =>
       val internalBytes = ByteString.fromArray(InternalNode.toProto(n).toByteArray)
       val
     case LeafNode(key, value) =>
     case ShadowNode(hash, height, balance) =>
     case _ =>
   }
 }

}