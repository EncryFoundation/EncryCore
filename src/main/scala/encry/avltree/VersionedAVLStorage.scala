package encry.avltree

import com.google.common.primitives.Ints
import com.typesafe.scalalogging.StrictLogging
import encry.avltree.VersionedAVLStorage.{InternalNodePrefix, LeafPrefix}
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.VersionalLevelDbKey
import encry.storage.levelDb.versionalLevelDB.{VersionalLevelDB, VersionalLevelDBCompanion}
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.encryfoundation.common.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ADKey, ADValue, Balance}
import scorex.crypto.hash
import scorex.crypto.hash.{CryptographicHash, Digest}
import scala.util.{Failure, Try}

case class NodeParameters(keySize: Int, valueSize: Option[Int], labelSize: Int)

//store keySize = 33!!
case class VersionedAVLStorage[D <: Digest](store: VersionalStorage,
                                            nodeParameters: NodeParameters,
                                            settings: EncryAppSettings)(implicit val hf: CryptographicHash[D])
  extends StrictLogging {

  val TopNodeKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(nodeParameters.labelSize)(123: Byte))
  val TopNodeHeight: ByteArrayWrapper = ByteArrayWrapper(Array.fill(nodeParameters.labelSize)(124: Byte))

  def rollback(version: ADDigest): Try[(EncryProverNodes[D], Int)] = Try {
    store.rollbackTo(StorageVersion @@ version.untag(ADDigest))
    val top: EncryProverNodes[D] =
      VersionedAVLStorage.fetch[D](ADKey @@ store.get(StorageKey @@ TopNodeKey.data).get.untag(StorageValue))(hf, store, nodeParameters)
    val topHeight: Int = Ints.fromByteArray(store.get(StorageKey @@ TopNodeHeight.data).get)
    top -> topHeight
  }.recoverWith { case e =>
    logger.info(s"Failed to recover tree for digest ${Algos.encode(version)} : $e")
    Failure(e)
  }

  //check what kind of storage is used. IODB haven't got initial version, instead of vldb
  def version: Option[ADDigest] = {
    val currentStorageVersion = store.currentVersion
    settings.storage.state match {
      case VersionalStorage.IODB =>
        if (currentStorageVersion sameElements IODBWrapper.initVer) None
        else Some(ADDigest @@ currentStorageVersion.untag(StorageVersion))
      case VersionalStorage.LevelDB =>
        if (currentStorageVersion sameElements VersionalLevelDBCompanion.INIT_VERSION(33)) None
        else Some(ADDigest @@ currentStorageVersion.untag(StorageVersion))
    }
  }

  def isEmpty: Boolean = version.isEmpty

  //todo: get key szie from settings
  def rollbackVersions: Iterable[ADDigest] = store.versions.map(ADDigest @@ _.untag(StorageVersion))
    .filter(elem => ByteArrayWrapper(elem) == VersionalLevelDBCompanion.INIT_VERSION(33))

  def update[K <: Array[Byte], V <: Array[Byte]](prover: BatchAVLProver[D, _],
                                                 additionalData: Seq[(K, V)]): Try[Unit] = Try {
    val digestWrapper: Store.K = ByteArrayWrapper(prover.digest)
    val indexes: Seq[(Store.K, Store.K)] = Seq(TopNodeKey -> nodeKey(prover.topNode),
      TopNodeHeight -> ByteArrayWrapper(Ints.toByteArray(prover.rootNodeHeight)))
    val toRemove: List[Store.K] = prover.removedNodes().map(rn => ByteArrayWrapper(rn.label))
    val toUpdate: Seq[(Store.K, Store.K)] = indexes ++ serializedVisitedNodes(prover.topNode, isTop = true)
    val toUpdateWrapped: Seq[(Store.K, Store.K)] =
      additionalData.map { case (k, v) => ByteArrayWrapper(k) -> ByteArrayWrapper(v) }
    val toUpdateWithWrapped: Seq[(ByteArrayWrapper, ByteArrayWrapper)] = toUpdate ++ toUpdateWrapped
    val toRemoveMerged: List[ByteArrayWrapper] = toRemove.filterNot(toUpdate.map(_._1).intersect(toRemove).contains)
    logger.info(s"Update storage to version ${Algos.encode(digestWrapper.data)}: ${toUpdateWithWrapped.size} elements to insert," +
      s" ${toRemove.size} elements to remove")
    store.insert(
      StorageVersion @@ digestWrapper.data,
      toUpdateWithWrapped.map{case (key, value) => StorageKey @@ key.data -> StorageValue @@ value.data}.toList,
      toRemoveMerged.map(key => StorageKey @@ key.data)
    )
  }.recoverWith { case e =>
    logger.info(s"Failed to update tree: $e")
    Failure(e)
  }

  // Should always serialize top node. It may not be new if it is the creation of the tree
  private def serializedVisitedNodes(node: EncryProverNodes[D], isTop: Boolean): Seq[(ByteArrayWrapper, ByteArrayWrapper)] =
    if (node.isNew || isTop) {
      val pair: (ByteArrayWrapper, ByteArrayWrapper) = (nodeKey(node), ByteArrayWrapper(toBytes(node)))
      node match {
        case n: InternalProverEncryNode[D] =>
          pair +: (serializedVisitedNodes(n.left, isTop = false) ++ serializedVisitedNodes(n.right, isTop = false))
        case _: ProverLeaf[D] => Seq(pair)
      }
    } else Seq()

  private def nodeKey(node: EncryProverNodes[D]): ByteArrayWrapper = ByteArrayWrapper(node.label)

  private def toBytes(node: EncryProverNodes[D]): Array[Byte] = node match {
    case n: InternalProverEncryNode[D] => InternalNodePrefix +: n.balance +: (n.key ++ n.left.label ++ n.right.label)
    case n: ProverLeaf[D] if nodeParameters.valueSize.isDefined => LeafPrefix +: (n.key ++ n.value ++ n.nextLeafKey)
    case n: ProverLeaf[D] => LeafPrefix +: (n.key ++ Ints.toByteArray(n.value.length) ++ n.value ++ n.nextLeafKey)
  }
}

object VersionedAVLStorage extends StrictLogging {

  val InternalNodePrefix: Byte = 0.toByte
  val LeafPrefix: Byte = 1.toByte

  def fetch[D <: hash.Digest](key: ADKey)(implicit hf: CryptographicHash[D],
                                          store: VersionalStorage,
                                          nodeParameters: NodeParameters): EncryProverNodes[D] = {
    val bytes: Array[Byte] = store.get(StorageKey @@ key.untag(ADKey)).get
    val keySize: Int = nodeParameters.keySize
    val labelSize: Int = nodeParameters.labelSize
    bytes.head match {
      case InternalNodePrefix =>
        val balance: Balance = Balance @@ bytes.slice(1, 2).head
        val key: ADKey = ADKey @@ bytes.slice(2, 2 + keySize)
        val leftKey: ADKey = ADKey @@ bytes.slice(2 + keySize, 2 + keySize + labelSize)
        val rightKey: ADKey = ADKey @@ bytes.slice(2 + keySize + labelSize, 2 + keySize + (2 * labelSize))
        val n: ProxyInternalProverEncryNode[D] = new ProxyInternalProverEncryNode[D](key, leftKey, rightKey, balance)
        n.isNew = false
        n
      case LeafPrefix =>
        val key: ADKey = ADKey @@ bytes.slice(1, 1 + keySize)
        val (value: ADValue, nextLeafKey: ADKey) = if (nodeParameters.valueSize.isDefined) {
          val valueSize: Int = nodeParameters.valueSize.get
          val value: ADValue = ADValue @@ bytes.slice(1 + keySize, 1 + keySize + valueSize)
          val nextLeafKey: ADKey = ADKey @@ bytes.slice(1 + keySize + valueSize, 1 + (2 * keySize) + valueSize)
          value -> nextLeafKey
        } else {
          val valueSize: Int = Ints.fromByteArray(bytes.slice(1 + keySize, 1 + keySize + 4))
          val value: ADValue = ADValue @@ bytes.slice(1 + keySize + 4, 1 + keySize + 4 + valueSize)
          val nextLeafKey: ADKey = ADKey @@ bytes.slice(1 + keySize + 4 + valueSize, 1 + (2 * keySize) + 4 + valueSize)
          value -> nextLeafKey
        }
        val l: ProverLeaf[D] = new ProverLeaf[D](key, value, nextLeafKey)
        l.isNew = false
        l
    }
  }

  class ProxyInternalProverEncryNode[D <: Digest](protected var pk: ADKey,
                                                  val lkey: ADKey,
                                                  val rkey: ADKey,
                                                  protected var pb: Balance = Balance @@ 0.toByte)
                                                 (implicit val phf: CryptographicHash[D],
                                                  store: VersionalStorage,
                                                  nodeParameters: NodeParameters)

    extends InternalProverEncryNode(k = pk, l = null, r = null, b = pb)(phf) {

    override def left: EncryProverNodes[D] = {
      if (l == null) l = VersionedAVLStorage.fetch[D](lkey)
      l
    }

    override def right: EncryProverNodes[D] = {
      if (r == null) r = VersionedAVLStorage.fetch[D](rkey)
      r
    }
  }

}