package encry.view.state.avlTree.utils.implicits

import encry.storage.VersionalStorage.StorageKey
import org.encryfoundation.common.utils.Algos

object Instances {

  type Hashable[A] = encry.view.state.avlTree.utils.implicits.Hashable[A]

  implicit val arrayHashable: Hashable[Array[Byte]] = new Hashable[Array[Byte]] {
    override def hash(value: Array[Byte]): Array[Byte] = Algos.hash(value)
  }

  implicit val arrayConvert: ConvertableToStorage[Array[Byte]] = new ConvertableToStorage[Array[Byte]] {
    override def convertToStorage(firstValue: StorageKey): Array[Byte] = firstValue
  }
}
