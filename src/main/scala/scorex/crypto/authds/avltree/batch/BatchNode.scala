package scorex.crypto.authds.avltree.batch

import scorex.crypto.authds.{ADKey, ADValue, Balance}
import scorex.crypto.hash._

sealed trait Node[D <: Digest] extends ToStringHelper {

  var visited: Boolean = false

  protected def computeLabel: D

  protected var labelOpt: Option[D] = None

  def label: D = labelOpt match {
    case None =>
      val l = computeLabel
      labelOpt = Some(l)
      l
    case Some(l) =>
      l
  }
}

sealed trait ProverNodes[D <: Digest] extends Node[D] with KeyInVar {
  var isNew: Boolean = true
}

sealed trait VerifierNodes[D <: Digest] extends Node[D]

class LabelOnlyNode[D <: Digest](l: D) extends VerifierNodes[D] {
  labelOpt = Some(l)

  protected def computeLabel: D = l
}

sealed trait InternalNode[D <: Digest] extends Node[D] {
  protected var b: Balance

  protected val hf: CryptographicHash[D]

  protected def computeLabel: D = hf.prefixedHash(1: Byte, Array(b), left.label, right.label)

  def balance: Balance = b

  def left: Node[D]

  def right: Node[D]

  def getNew(newLeft: Node[D] = left, newRight: Node[D] = right, newBalance: Balance = b): InternalNode[D]

  def getNewKey(newKey: ADKey): InternalNode[D]
}

class InternalProverNode[D <: Digest](protected var k: ADKey,
                                      protected var l: ProverNodes[D],
                                      protected var r: ProverNodes[D],
                                      protected var b: Balance = Balance @@ 0.toByte)(implicit val hf: CryptographicHash[D])
  extends ProverNodes[D] with InternalNode[D] {


  override def left: ProverNodes[D] = l

  override def right: ProverNodes[D] = r

  def getNewKey(newKey: ADKey): InternalProverNode[D] = {
    if (isNew) {
      k = newKey // label doesn't change when key of an internal node changes
      this
    } else {
      val ret = new InternalProverNode(newKey, left, right, b)
      ret.labelOpt = labelOpt // label doesn't change when key of an internal node changes
      ret
    }
  }

  def getNew(newLeft: Node[D] = left, newRight: Node[D] = right, newBalance: Balance = b): InternalProverNode[D] = {
    if (isNew) {
      l = newLeft.asInstanceOf[ProverNodes[D]]
      r = newRight.asInstanceOf[ProverNodes[D]]
      b = newBalance
      labelOpt = None
      this
    } else {
      new InternalProverNode(k, newLeft.asInstanceOf[ProverNodes[D]], newRight.asInstanceOf[ProverNodes[D]], newBalance)
    }
  }

  override def toString: String = {
    s"${arrayToString(label)}: ProverNode(${arrayToString(key)}, ${arrayToString(left.label)}, " +
      s"${arrayToString(right.label)}, $balance)"
  }
}

class InternalVerifierNode[D <: Digest](protected var l: Node[D], protected var r: Node[D], protected var b: Balance)
                                       (implicit val hf: CryptographicHash[D]) extends VerifierNodes[D] with InternalNode[D] {


  override def left: Node[D] = l

  override def right: Node[D] = r

  def getNewKey(newKey: ADKey): InternalNode[D] = this

  def getNew(newLeft: Node[D] = l, newRight: Node[D] = r, newBalance: Balance = b): InternalVerifierNode[D] = {
    l = newLeft
    r = newRight
    b = newBalance
    labelOpt = None
    this
  }

  override def toString: String = {
    s"${arrayToString(label)}: VerifierNode(${arrayToString(left.label)}, ${arrayToString(right.label)}, $balance)"
  }
}

sealed trait Leaf[D <: Digest] extends Node[D] with KeyInVar {
  protected var nk: ADKey
  protected var v: ADValue


  def nextLeafKey: ADKey = nk

  def value: ADValue = v

  protected val hf: CryptographicHash[D]

  protected def computeLabel: D = hf.prefixedHash(0: Byte, k, v, nk)

  def getNew(newKey: ADKey = k, newValue: ADValue = v, newNextLeafKey: ADKey = nk): Leaf[D]

  override def toString: String = {
    s"${arrayToString(label)}: Leaf(${arrayToString(key)}, ${arrayToString(value)}, ${arrayToString(nextLeafKey)})"
  }
}

class VerifierLeaf[D <: Digest](protected var k: ADKey, protected var v: ADValue, protected var nk: ADKey)
                               (implicit val hf: CryptographicHash[D]) extends Leaf[D] with VerifierNodes[D] {

  def getNew(newKey: ADKey = k, newValue: ADValue = v, newNextLeafKey: ADKey = nk): VerifierLeaf[D] = {
    k = newKey
    v = newValue
    nk = newNextLeafKey
    labelOpt = None
    this
  }
}

class ProverLeaf[D <: Digest](protected var k: ADKey, protected var v: ADValue, protected var nk: ADKey)
                             (implicit val hf: CryptographicHash[D]) extends Leaf[D] with ProverNodes[D] {

  def getNew(newKey: ADKey = k, newValue: ADValue = v, newNextLeafKey: ADKey = nk): ProverLeaf[D] = {
    if (isNew) {
      k = newKey
      v = newValue
      nk = newNextLeafKey
      labelOpt = None
      this
    } else {
      new ProverLeaf(newKey, newValue, newNextLeafKey)
    }
  }
}

trait KeyInVar {
  protected var k: ADKey

  def key: ADKey = k
}

