package encry.avltree

import com.google.common.primitives.Ints
import org.encryfoundation.common.utils.TaggedTypes
import org.encryfoundation.common.utils.TaggedTypes.{ADValue, _}
import scorex.crypto.hash._
import scorex.utils.ByteArray
import supertagged.@@
import scala.collection.mutable
import scala.util.{Failure, Try}

class BatchAVLVerifier[D <: Digest, HF <: CryptographicHash[D]](startingDigest: ADDigest,
                                                                proof: SerializedAdProof,
                                                                override val keyLength: Int,
                                                                override val valueLengthOpt: Option[Int],
                                                                maxNumOperations: Option[Int] = None,
                                                                maxDeletes: Option[Int] = None)
                                                               (implicit hf: HF = Blake2b256)
  extends AuthenticatedTreeOps[D] {

  override val collectChangedNodes: Boolean = false

  protected val labelLength: Int = hf.DigestSize

  def digest: Option[ADDigest] = topNode.map(digest(_))

  private var directionsIndex = 0
  private var lastRightStep = 0
  private var replayIndex = 0 // Keeps track of where we are when replaying directions

  protected def nextDirectionIsLeft(key: ADKey, r: InternalEncryNode[D]): Boolean = {
    val ret: Boolean = if ((proof(directionsIndex >> 3) & (1 << (directionsIndex & 7)).toByte) != 0) {
      true
    } else {
      lastRightStep = directionsIndex
      false
    }
    directionsIndex += 1
    ret
  }

  protected def keyMatchesLeaf(key: ADKey, r: EncryLeaf[D]): Boolean = {
    val c: Int = ByteArray.compare(key, r.key)
    require(c >= 0)
    if (c == 0) {
      true
    } else {
      require(ByteArray.compare(key, r.nextLeafKey) < 0)
      false
    }
  }

  protected def replayComparison: Int = {
    val ret: Int = if (replayIndex == lastRightStep) {
      0
    } else if ((proof(replayIndex >> 3) & (1 << (replayIndex & 7)).toByte) == 0 && replayIndex < lastRightStep) {
      1
    } else {
      -1
    }
    replayIndex += 1
    ret
  }

  protected def addNode(r: EncryLeaf[D], key: ADKey, v: ADValue): InternalVerifierEncryNode[D] = {
    val n: ADKey = r.nextLeafKey
    new InternalVerifierEncryNode(r.getNew(newNextLeafKey = key), new VerifierLeaf(key, v, n), Balance @@ 0.toByte)
  }

  protected var rootNodeHeight = 0

  private lazy val reconstructedTree: Option[VerifierNodes[D]] = Try {
    require(labelLength > 0)
    require(keyLength > 0)
    valueLengthOpt.foreach(vl => require(vl >= 0))
    require(startingDigest.length == labelLength + 1)
    rootNodeHeight = startingDigest.last & 0xff

    val maxNodes = if (maxNumOperations.isDefined) {
      var logNumOps: Int = 0
      var temp: Int = 1
      val realNumOperations: Int = maxNumOperations.getOrElse(0)
      while (temp < realNumOperations) {
        temp = temp * 2
        logNumOps += 1
      }

      temp = 1 + math.max(rootNodeHeight, logNumOps)
      val hnew: Int = temp + temp / 2 // this will replace 1.4405 from the paper with 1.5 and will round down, which is safe, because hnew is an integer
      val realMaxDeletes: Int = maxDeletes.getOrElse(realNumOperations)
      (realNumOperations + realMaxDeletes) * (2 * rootNodeHeight + 1) + realMaxDeletes * hnew + 1 // +1 needed in case numOperations == 0
    } else {
      0
    }

    var numNodes = 0
    val s = new mutable.Stack[VerifierNodes[D]] // Nodes and depths
    var i: Int = 0
    var previousLeaf: Option[EncryLeaf[D]] = None
    while (proof(i) != EndOfTreeInPackagedProof) {
      val n: Byte = proof(i)
      i += 1
      numNodes += 1
      require(maxNumOperations.isEmpty || numNodes <= maxNodes, "Proof too long")
      n match {
        case LabelInPackagedProof =>
          val label: D = proof.slice(i, i + labelLength).asInstanceOf[D]
          i += labelLength
          s.push(new LabelOnlyEncryNode[D](label))
          previousLeaf = None
        case LeafInPackagedProof =>
          val key: Array[Byte] @@ TaggedTypes.ADKey.Tag = if (previousLeaf.nonEmpty) {
            ADKey @@ previousLeaf.get.nextLeafKey
          }
          else {
            val start: Int = i
            i += keyLength
            ADKey @@ proof.slice(start, i)
          }
          val nextLeafKey: ADKey = ADKey @@ proof.slice(i, i + keyLength)
          i += keyLength
          val valueLength: Int = valueLengthOpt.getOrElse {
            val vl: Int = Ints.fromByteArray(proof.slice(i, i + 4))
            i += 4
            vl
          }
          val value: ADValue = ADValue @@ proof.slice(i, i + valueLength)
          i += valueLength
          val leaf: VerifierLeaf[D] = new VerifierLeaf[D](key, value, nextLeafKey)
          s.push(leaf)
          previousLeaf = Some(leaf)
        case _ =>
          val right: VerifierNodes[D] = s.pop
          val left: VerifierNodes[D] = s.pop
          s.push(new InternalVerifierEncryNode(left, right, Balance @@ n))
      }
    }

    require(s.size == 1)
    val root: VerifierNodes[D] = s.pop
    require(startingDigest startsWith root.label)
    directionsIndex = (i + 1) * 8 // Directions start right after the packed tree, which we just finished
    Some(root)
  }.recoverWith { case e =>
    e.printStackTrace()
    Failure(e)
  }.getOrElse(None)

  private var topNode: Option[VerifierNodes[D]] = reconstructedTree

  def performOneOperation(operation: Operation): Try[Option[ADValue]] = Try {
    replayIndex = directionsIndex
    val operationResult: Try[(EncryNode[D], Option[ADValue])] = returnResultOfOneOperation(operation, topNode.get)
    topNode = operationResult.map(s => Some(s._1.asInstanceOf[VerifierNodes[D]])).getOrElse(None)
    operationResult.get._2
  }

  override def toString: String = {

    def stringTreeHelper(rNode: VerifierNodes[D], depth: Int): String =
      Seq.fill(depth + 2)(" ").mkString + (rNode match {
        case leaf: VerifierLeaf[D] =>
          "At leaf label = " + arrayToString(leaf.label) + " key = " + arrayToString(leaf.key) +
            " nextLeafKey = " + arrayToString(leaf.nextLeafKey) + "value = " + leaf.value + "\n"
        case r: InternalVerifierEncryNode[D] =>
          "Internal node label = " + arrayToString(r.label) + " balance = " +
            r.balance + "\n" + stringTreeHelper(r.left.asInstanceOf[VerifierNodes[D]], depth + 1) +
            stringTreeHelper(r.right.asInstanceOf[VerifierNodes[D]], depth + 1)
        case n: LabelOnlyEncryNode[D] =>
          "Label-only node label = " + arrayToString(n.label) + "\n"
      })

    topNode match {
      case None => "None"
      case Some(t) => stringTreeHelper(t, 0)
    }
  }
}
