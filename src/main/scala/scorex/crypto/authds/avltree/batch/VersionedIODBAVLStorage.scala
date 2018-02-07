package scorex.crypto.authds.avltree.batch

import com.google.common.primitives.Ints
import io.iohk.iodb.Store.{K, V}
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.crypto.authds.avltree.batch.VersionedIODBAVLStorage.{InternalNodePrefix, LeafPrefix}
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, Balance}
import scorex.crypto.encode.Base58
import scorex.crypto.hash
import scorex.crypto.hash.{Digest, ThreadUnsafeHash}
import scorex.utils.ScryptoLogging

import scala.util.{Failure, Try}

class VersionedIODBAVLStorage[D <: Digest](store: Store, nodeParameters: NodeParameters)
                                          (implicit val hf: ThreadUnsafeHash[D])
  extends VersionedAVLStorage[D] with ScryptoLogging {

  private lazy val labelSize = nodeParameters.labelSize

  private val TopNodeKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(labelSize)(123: Byte))
  private val TopNodeHeight: ByteArrayWrapper = ByteArrayWrapper(Array.fill(labelSize)(124: Byte))
  private val DigestLength = 33
  private val InitialVersion = ADDigest @@ Array.fill(DigestLength)(11: Byte)

  override def update(prover: BatchAVLProver[D, _]): Try[Unit] = update(prover, Seq())

  override def rollback(version: ADDigest): Try[(ProverNodes[D], Int)] = Try {
    store.rollback(ByteArrayWrapper(version))

    val top = VersionedIODBAVLStorage.fetch[D](ADKey @@ store.get(TopNodeKey).get.data)(hf, store, nodeParameters)
    val topHeight = Ints.fromByteArray(store.get(TopNodeHeight).get.data)

    top -> topHeight
  }.recoverWith { case e =>
    log.warn("Failed to recover tree", e)
    Failure(e)
  }

  override def version: Option[ADDigest] = store.lastVersionID.map(d => ADDigest @@ d.data)

  def rollbackVersions: Iterable[ADDigest] = store.rollbackVersions().map(d => ADDigest @@ d.data)

  def leafsIterator(): Iterator[(K, V)] = store.getAll().filter { case (_, v) => v.data.head == LeafPrefix }

  private def serializedVisitedNodes(node: ProverNodes[D]): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    if (node.isNew) {
      val pair: (ByteArrayWrapper, ByteArrayWrapper) = (nodeKey(node), ByteArrayWrapper(toBytes(node)))
      node match {
        case n: InternalProverNode[D] =>
          val leftSubtree = serializedVisitedNodes(n.left)
          val rightSubtree = serializedVisitedNodes(n.right)
          pair +: (leftSubtree ++ rightSubtree)
        case _: ProverLeaf[D] => Seq(pair)
      }
    } else Seq()
  }

  //TODO label or key???
  private def nodeKey(node: ProverNodes[D]): ByteArrayWrapper = ByteArrayWrapper(node.label)

  private def toBytes(node: ProverNodes[D]): Array[Byte] = node match {
    case n: InternalProverNode[D] => InternalNodePrefix +: n.balance +: (n.key ++ n.left.label ++ n.right.label)
    case n: ProverLeaf[D] => LeafPrefix +: (n.key ++ Ints.toByteArray(n.value.length) ++ n.value ++ n.nextLeafKey)
  }

  override def update[K <: Array[Byte], V <: Array[Byte]](prover: BatchAVLProver[D, _],
                                                          additionalData: Seq[(K, V)]): Try[Unit] = Try {
    //TODO topNode is a special case?
    val topNode = prover.topNode
    val key = nodeKey(topNode)
    val topNodePair = (key, ByteArrayWrapper(toBytes(topNode)))
    val digestWrapper = ByteArrayWrapper(prover.digest)
    val indexes = Seq(TopNodeKey -> key, TopNodeHeight -> ByteArrayWrapper(Ints.toByteArray(prover.rootNodeHeight)))
    val toInsert = serializedVisitedNodes(topNode)
    log.trace(s"Put(${store.lastVersionID}) ${toInsert.map(k => Base58.encode(k._1.data))}")
    val toUpdate = if (!toInsert.map(_._1).contains(key)) {
      topNodePair +: (indexes ++ toInsert)
    } else indexes ++ toInsert

    log.info(toUpdate.size + " elements to insert into db")

    val toUpdateWrapped = additionalData.map{case (k, v) =>
      ByteArrayWrapper(k) -> ByteArrayWrapper(v)
    }

    //TODO toRemove list?
    store.update(digestWrapper, toRemove = Seq(), toUpdate ++ toUpdateWrapped)
  }.recoverWith { case e =>
    log.warn("Failed to update tree", e)
    Failure(e)
  }
}


object VersionedIODBAVLStorage {
  val InternalNodePrefix: Byte = 0: Byte
  val LeafPrefix: Byte = 1: Byte

  def fetch[D <: hash.Digest](key: ADKey)(implicit hf: ThreadUnsafeHash[D],
                                          store: Store,
                                          nodeParameters: NodeParameters): ProverNodes[D] = {
    val bytes = store(ByteArrayWrapper(key)).data
    lazy val keySize = nodeParameters.keySize
    lazy val labelSize = nodeParameters.labelSize

    bytes.head match {
      case InternalNodePrefix =>
        val balance = Balance @@ bytes.slice(1, 2).head
        val key = ADKey @@ bytes.slice(2, 2 + keySize)
        val leftKey = ADKey @@ bytes.slice(2 + keySize, 2 + keySize + labelSize)
        val rightKey = ADKey @@ bytes.slice(2 + keySize + labelSize, 2 + keySize + (2 * labelSize))

        val n = new ProxyInternalProverNode[D](key, leftKey, rightKey, balance)
        n.isNew = false
        n
      case LeafPrefix =>
        val key = ADKey @@ bytes.slice(1, 1 + keySize)
        val valueLength = Ints.fromByteArray(bytes.slice(1 + keySize, 1 + keySize + 4))
        val value = ADValue @@ bytes.slice(1 + keySize + 4, 1 + keySize + 4 + valueLength)
        val nextLeafKey = ADKey @@ bytes.slice(1 + keySize + 4 + valueLength, 1 + (2 * keySize) + 4 + valueLength)
        val l = new ProverLeaf[D](key, value, nextLeafKey)
        l.isNew = false
        l
    }
  }
}
