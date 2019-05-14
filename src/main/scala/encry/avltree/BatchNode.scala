package encry.avltree

import com.typesafe.scalalogging.StrictLogging
import org.encryfoundation.common.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, ADValue, Balance}
import scorex.crypto.encode.Base16
import scorex.crypto.hash._

sealed trait EncryNode[D <: Digest] extends StrictLogging{

  var visited: Boolean = false
  var labelOpt: Option[D] = None

  protected def arrayToString(a: Array[Byte]): String = Base16.encode(a).take(8)

  protected def computeLabel: D

  def label: D = labelOpt match {
    case None =>
      val l: D = computeLabel
      labelOpt = Some(l)
      l
    case Some(l) => l
  }
}

sealed trait EncryProverNodes[D <: Digest] extends EncryNode[D] with KeyInVar {
  var isNew: Boolean = true
}

sealed trait VerifierNodes[D <: Digest] extends EncryNode[D]

class LabelOnlyEncryNode[D <: Digest](l: D) extends VerifierNodes[D] {

  labelOpt = Some(l)

  protected def computeLabel: D = {
    logger.info("computeLabel in LabelOnlyEncryNode")
    l
  }
}

sealed trait InternalEncryNode[D <: Digest] extends EncryNode[D] {
  protected var b: Balance

  protected val hf: CryptographicHash[D]

  protected def computeLabel: D = {
    logger.info("computeLabel in InternalEncryNode")
    hf.prefixedHash(1: Byte, Array(b), left.label, right.label)
  }

  def balance: Balance = b

  def left: EncryNode[D]

  def right: EncryNode[D]

  def getNew(newLeft: EncryNode[D] = left, newRight: EncryNode[D] = right, newBalance: Balance = b): InternalEncryNode[D]

  def getNewKey(newKey: ADKey): InternalEncryNode[D]
}

class InternalProverEncryNode[D <: Digest](protected var k: ADKey,
                                           protected var l: EncryProverNodes[D],
                                           protected var r: EncryProverNodes[D],
                                           protected var b: Balance = Balance @@ 0.toByte)
                                          (implicit val hf: CryptographicHash[D])
  extends EncryProverNodes[D] with InternalEncryNode[D] {

  override def left: EncryProverNodes[D] = l

  override def right: EncryProverNodes[D] = r

  def getNewKey(newKey: ADKey): InternalProverEncryNode[D] = if (isNew) {
    k = newKey // label doesn't change when key of an internal node changes
    this
  } else {
    val ret: InternalProverEncryNode[D] = new InternalProverEncryNode(newKey, left, right, b)
    logger.info("getNewKey")
    ret.labelOpt = labelOpt // label doesn't change when key of an internal node changes
    ret
  }

  def getNew(newLeft: EncryNode[D] = left, newRight: EncryNode[D] = right,
             newBalance: Balance = b): InternalProverEncryNode[D] = if (isNew) {
    l = newLeft.asInstanceOf[EncryProverNodes[D]]
    r = newRight.asInstanceOf[EncryProverNodes[D]]
    b = newBalance
    //logger.info("getNew1")
    labelOpt = None
    this
  } else new InternalProverEncryNode(k, newLeft.asInstanceOf[EncryProverNodes[D]],
    newRight.asInstanceOf[EncryProverNodes[D]], newBalance)

  override def toString: String = s"${arrayToString(label)}: ProverNode(${arrayToString(key)}, " +
    s"${arrayToString(left.label)}, ${arrayToString(right.label)}, $balance)"
}

class InternalVerifierEncryNode[D <: Digest](protected var l: EncryNode[D], protected var r: EncryNode[D],
                                             protected var b: Balance)
                                            (implicit val hf: CryptographicHash[D])
  extends VerifierNodes[D] with InternalEncryNode[D] {

  override def left: EncryNode[D] = l

  override def right: EncryNode[D] = r

  def getNewKey(newKey: ADKey): InternalEncryNode[D] = this

  def getNew(newLeft: EncryNode[D] = l, newRight: EncryNode[D] = r,
             newBalance: Balance = b): InternalVerifierEncryNode[D] = {
    l = newLeft
    r = newRight
    b = newBalance
    //logger.info("getNew2")
    labelOpt = None
    this
  }

  override def toString: String =
    s"${arrayToString(label)}: VerifierNode(${arrayToString(left.label)}, ${arrayToString(right.label)}, $balance)"
}

sealed trait EncryLeaf[D <: Digest] extends EncryNode[D] with KeyInVar {
  protected var nk: ADKey
  protected var v: ADValue
  protected val hf: CryptographicHash[D]

  def nextLeafKey: ADKey = nk

  def value: ADValue = v

  protected def computeLabel: D = {
    logger.info("compute label in EncryLeaf")
    hf.prefixedHash(0: Byte, k, v, nk)
  }

  def getNew(newKey: ADKey = k, newValue: ADValue = v, newNextLeafKey: ADKey = nk): EncryLeaf[D]

  override def toString: String =
    s"${arrayToString(label)}: Leaf(${arrayToString(key)}, ${arrayToString(value)}, ${arrayToString(nextLeafKey)})"
}

class VerifierLeaf[D <: Digest](protected var k: ADKey, protected var v: ADValue, protected var nk: ADKey)
                               (implicit val hf: CryptographicHash[D]) extends EncryLeaf[D] with VerifierNodes[D] {

  def getNew(newKey: ADKey = k, newValue: ADValue = v, newNextLeafKey: ADKey = nk): VerifierLeaf[D] = {
    k = newKey
    v = newValue
    nk = newNextLeafKey
    //logger.info("getnew3")
    labelOpt = None
    this
  }
}

class ProverLeaf[D <: Digest](protected var k: ADKey,
                              protected var v: ADValue,
                              protected var nk: ADKey)
                             (implicit val hf: CryptographicHash[D]) extends EncryLeaf[D] with EncryProverNodes[D] {
  def getNew(newKey: ADKey = k, newValue: ADValue = v, newNextLeafKey: ADKey = nk): ProverLeaf[D] = if (isNew) {
    k = newKey
    v = newValue
    nk = newNextLeafKey
    //logger.info("getnew4")
    labelOpt = None
    this
  } else new ProverLeaf(newKey, newValue, newNextLeafKey)
}

trait KeyInVar {
  protected var k: ADKey

  def key: ADKey = k
}

