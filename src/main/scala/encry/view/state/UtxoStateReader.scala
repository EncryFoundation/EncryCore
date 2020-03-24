package encry.view.state

import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.state.avlTree.{AvlTree, Node}
import encry.view.state.avlTree.utils.implicits.Instances._
import org.encryfoundation.common.modifiers.state.StateModifierSerializer
import org.encryfoundation.common.modifiers.state.box.EncryBaseBox
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, Height}

import scala.util.Try

trait UtxoStateReader {

  implicit val hf: Algos.HF = Algos.hash

  def version: VersionTag

  def rootNode: Node[StorageKey, StorageValue]

  //todo remove
  def avlStorage: VersionalStorage

  def rootHash: Array[Byte]

  def stateSafePointHeight: Height

  def getOperationsRootHash(toInsert: List[(StorageKey, StorageValue)],
                            toDelete: List[StorageKey]): Try[Array[Byte]]

  def boxById(boxId: ADKey): Option[EncryBaseBox]

  def boxesByIds(ids: Seq[ADKey]): Seq[EncryBaseBox]

  def typedBoxById[B <: EncryBaseBox](boxId: ADKey): Option[EncryBaseBox]

  def safePointHeight: Height
}

object UtxoStateReader {

  def apply(state: UtxoState): UtxoStateReader = new UtxoStateReader {
    override def version: VersionTag = state.version
    override def stateSafePointHeight: Height = state.safePointHeight
    override def boxById(boxId: ADKey): Option[EncryBaseBox] = state.boxById(boxId)
    override def boxesByIds(ids: Seq[ADKey]): Seq[EncryBaseBox] = state.boxesByIds(ids)
    override def typedBoxById[B <: EncryBaseBox](boxId: ADKey): Option[EncryBaseBox] = state.typedBoxById[B](boxId)
    override def rootNode: Node[StorageKey, StorageValue] = state.rootNode
    override def avlStorage: VersionalStorage = state.avlStorage
    override def rootHash: Array[Byte] = state.rootHash
    override def safePointHeight: Height = state.safePointHeight
    override def getOperationsRootHash(toInsert: List[(StorageKey, StorageValue)],
                                       toDelete: List[StorageKey]): Try[Array[Byte]] = state.getOperationsRootHash(toInsert, toDelete)
  }
}