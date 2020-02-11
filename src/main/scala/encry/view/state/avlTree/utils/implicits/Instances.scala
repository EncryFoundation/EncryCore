package encry.view.state.avlTree.utils.implicits

import cats.Order
import cats.kernel.Monoid
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.view.state.avlTree.InternalNode
import org.bouncycastle.util.Arrays
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey

object Instances {

  type Hashable[A] = encry.view.state.avlTree.utils.implicits.Hashable[A]
  type Serializer[T] = encry.view.state.avlTree.utils.implicits.Serializer[T]

  implicit val arrayHashable: Hashable[Array[Byte]] = new Hashable[Array[Byte]] {
    override def hash(value: Array[Byte]): Array[Byte] = Algos.hash(value)
  }

//  implicit val arrSer: Serializer[Array[Byte]] = new Serializer[Array[Byte]] {
//    override def toBytes(elem: Array[Byte]): Array[Byte] = elem
//
//    override def fromBytes(bytes: Array[Byte]): Array[Byte] = bytes
//  }
//
//  implicit val arrMonoid: Monoid[Array[Byte]] = new Monoid[Array[Byte]] {
//    override def empty: Array[Byte] = Array.emptyByteArray
//
//    override def combine(x: Array[Byte], y: Array[Byte]): Array[Byte] = x ++ y
//  }

  implicit val storVerMonoid: Monoid[StorageValue] = new Monoid[StorageValue] {
    override def empty: StorageValue = StorageValue @@ Array.emptyByteArray

    override def combine(x: StorageValue, y: StorageValue): StorageValue = StorageValue @@ (x ++ y)
  }

  implicit def adKeyOrder: Order[StorageKey] = new Order[StorageKey] {
    override def compare(x: StorageKey, y: StorageKey): Int = BigInt(x) compare BigInt(y)
  }

  implicit val storVerSer: Serializer[StorageValue] = new Serializer[StorageValue] {
    override def toBytes(elem: StorageValue): Array[Byte] = elem

    override def fromBytes(bytes: Array[Byte]): StorageValue = StorageValue @@ bytes
  }

  implicit val storKeySer: Serializer[StorageKey] = new Serializer[StorageKey] {
    override def toBytes(elem: StorageKey): Array[Byte] = elem

    override def fromBytes(bytes: Array[Byte]): StorageKey = StorageKey @@ bytes
  }

  implicit val storKeyhash: Hashable[StorageKey] = new Hashable[StorageKey] {
    override def hash(value: StorageKey): Array[Byte] = Algos.hash(value)
  }

  implicit val storValuehash: Hashable[StorageValue] = new Hashable[StorageValue] {
    override def hash(value: StorageValue): Array[Byte] = Algos.hash(value)
  }

  implicit val storKeyMonoid: Monoid[StorageKey] = new Monoid[StorageKey] {
    override def empty: StorageKey = StorageKey @@ Array.emptyByteArray

    override def combine(x: StorageKey, y: StorageKey): StorageKey = StorageKey @@ (x ++ y)
  }

//  implicit val storVerMonoid: Monoid[StorageVersion] = new Monoid[StorageVersion] {
//    override def empty: StorageVersion = StorageVersion @@ Array.emptyByteArray
//
//    override def combine(x: StorageVersion, y: StorageVersion): StorageVersion = StorageVersion @@ (x ++ y)
//  }
//
//  implicit val storVerSer: Serializer[StorageVersion] = new Serializer[StorageVersion] {
//    override def toBytes(elem: StorageVersion): Array[Byte] = elem
//
//    override def fromBytes(bytes: Array[Byte]): StorageVersion = StorageVersion @@ bytes
//  }

  implicit val arrayConvert: ConvertableToStorage[Array[Byte]] = new ConvertableToStorage[Array[Byte]] {
    override def convertToStorage(firstValue: StorageKey): Array[Byte] = firstValue
  }
}
