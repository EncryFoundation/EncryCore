package scorex.crypto.authds.avltree.batch

import com.google.common.primitives.Ints
import scorex.crypto.authds._
import scorex.crypto.hash._
import scorex.utils.ByteArray

import scala.collection.mutable
import scala.util.{Failure, Try}

class BatchAVLVerifier[D <: Digest, HF <: CryptographicHash[D]](startingDigest: ADDigest,
                                                                proof: SerializedAdProof,
                                                                override val keyLength: Int,
                                                                override val valueLengthOpt: Option[Int],
                                                                maxNumOperations: Option[Int] = None,
                                                                maxDeletes: Option[Int] = None)
                                                               (implicit hf: HF = Blake2b256)
  extends AuthenticatedTreeOps[D] with ToStringHelper {

  override val collectChangedNodes: Boolean = false

  protected val labelLength: Int = hf.DigestSize

  def digest: Option[ADDigest] = topNode.map(digest(_))

  private var directionsIndex = 0
  private var lastRightStep = 0
  private var replayIndex = 0 // Keeps track of where we are when replaying directions

  protected def nextDirectionIsLeft(key: ADKey, r: InternalNode[D]): Boolean = {
    val ret = if ((proof(directionsIndex >> 3) & (1 << (directionsIndex & 7)).toByte) != 0) {
      true
    } else {
      lastRightStep = directionsIndex
      false
    }
    directionsIndex += 1
    ret
  }

  protected def keyMatchesLeaf(key: ADKey, r: Leaf[D]): Boolean = {
    val c = ByteArray.compare(key, r.key)
    require(c >= 0)
    if (c == 0) {
      true
    } else {
      require(ByteArray.compare(key, r.nextLeafKey) < 0)
      false
    }
  }

  protected def replayComparison: Int = {
    val ret = if (replayIndex == lastRightStep) {
      0
    } else if ((proof(replayIndex >> 3) & (1 << (replayIndex & 7)).toByte) == 0 && replayIndex < lastRightStep) {
      1
    } else {
      -1
    }
    replayIndex += 1
    ret
  }

  protected def addNode(r: Leaf[D], key: ADKey, v: ADValue): InternalVerifierNode[D] = {
    val n = r.nextLeafKey
    new InternalVerifierNode(r.getNew(newNextLeafKey = key), new VerifierLeaf(key, v, n), Balance @@ 0.toByte)
  }

  protected var rootNodeHeight = 0

  private lazy val reconstructedTree: Option[VerifierNodes[D]] = Try {
    require(labelLength > 0)
    require(keyLength > 0)
    valueLengthOpt.foreach(vl => require(vl >= 0))
    require(startingDigest.length == labelLength + 1)
    rootNodeHeight = startingDigest.last & 0xff

    val maxNodes = if (maxNumOperations.isDefined) {
      var logNumOps = 0
      var temp = 1
      val realNumOperations: Int = maxNumOperations.getOrElse(0)
      while (temp < realNumOperations) {
        temp = temp * 2
        logNumOps += 1
      }

      temp = 1 + math.max(rootNodeHeight, logNumOps)
      val hnew = temp + temp / 2 // this will replace 1.4405 from the paper with 1.5 and will round down, which is safe, because hnew is an integer
      val realMaxDeletes: Int = maxDeletes.getOrElse(realNumOperations)
      (realNumOperations + realMaxDeletes) * (2 * rootNodeHeight + 1) + realMaxDeletes * hnew + 1 // +1 needed in case numOperations == 0
    } else {
      0
    }

    var numNodes = 0
    val s = new mutable.Stack[VerifierNodes[D]] // Nodes and depths
    var i = 0
    var previousLeaf: Option[Leaf[D]] = None
    while (proof(i) != EndOfTreeInPackagedProof) {
      val n = proof(i)
      i += 1
      numNodes += 1
      require(maxNumOperations.isEmpty || numNodes <= maxNodes, "Proof too long")
      n match {
        case LabelInPackagedProof =>
          val label = proof.slice(i, i + labelLength).asInstanceOf[D]
          i += labelLength
          s.push(new LabelOnlyNode[D](label))
          previousLeaf = None
        case LeafInPackagedProof =>
          val key = if (previousLeaf.nonEmpty) {
            ADKey @@ previousLeaf.get.nextLeafKey
          }
          else {
            val start = i
            i += keyLength
            ADKey @@ proof.slice(start, i)
          }
          val nextLeafKey = ADKey @@ proof.slice(i, i + keyLength)
          i += keyLength
          val valueLength: Int = valueLengthOpt.getOrElse {
            val vl = Ints.fromByteArray(proof.slice(i, i + 4))
            i += 4
            vl
          }
          val value = ADValue @@ proof.slice(i, i + valueLength)
          i += valueLength
          val leaf = new VerifierLeaf[D](key, value, nextLeafKey)
          s.push(leaf)
          previousLeaf = Some(leaf)
        case _ =>
          val right = s.pop
          val left = s.pop
          s.push(new InternalVerifierNode(left, right, Balance @@ n))
      }
    }

    require(s.size == 1)
    val root = s.pop
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
    val operationResult = returnResultOfOneOperation(operation, topNode.get)
    topNode = operationResult.map(s => Some(s._1.asInstanceOf[VerifierNodes[D]])).getOrElse(None)
    operationResult.get._2
  }

  override def toString: String = {

    def stringTreeHelper(rNode: VerifierNodes[D], depth: Int): String =
      Seq.fill(depth + 2)(" ").mkString + (rNode match {
        case leaf: VerifierLeaf[D] =>
          "At leaf label = " + arrayToString(leaf.label) + " key = " + arrayToString(leaf.key) +
            " nextLeafKey = " + arrayToString(leaf.nextLeafKey) + "value = " + leaf.value + "\n"
        case r: InternalVerifierNode[D] =>
          "Internal node label = " + arrayToString(r.label) + " balance = " +
            r.balance + "\n" + stringTreeHelper(r.left.asInstanceOf[VerifierNodes[D]], depth + 1) +
            stringTreeHelper(r.right.asInstanceOf[VerifierNodes[D]], depth + 1)
        case n: LabelOnlyNode[D] =>
          "Label-only node label = " + arrayToString(n.label) + "\n"
      })

    topNode match {
      case None => "None"
      case Some(t) => stringTreeHelper(t, 0)
    }
  }

  def extractNodes(extractor: VerifierNodes[D] => Boolean): Option[Seq[VerifierNodes[D]]] = {
    def treeTraverser(rNode: VerifierNodes[D], collected: Seq[VerifierNodes[D]]): Seq[VerifierNodes[D]] = rNode match {
      case l: VerifierLeaf[D] => if (extractor(l)) l +: collected else collected
      case ln: LabelOnlyNode[D] => if (extractor(ln)) ln +: collected else collected
      case int: InternalVerifierNode[D] =>
        collected ++
          treeTraverser(int.right.asInstanceOf[VerifierNodes[D]], Seq()) ++
          treeTraverser(int.left.asInstanceOf[VerifierNodes[D]], Seq())
    }

    topNode.map(t => treeTraverser(t, Seq()))
  }

  def extractFirstNode(extractor: VerifierNodes[D] => Boolean): Option[VerifierNodes[D]] = {
    def treeTraverser(rNode: VerifierNodes[D]): Option[VerifierNodes[D]] = rNode match {
      case l: VerifierLeaf[D] => Some(l).filter(extractor)
      case ln: LabelOnlyNode[D] => Some(ln).filter(extractor)
      case int: InternalVerifierNode[D] =>
        treeTraverser(int.left.asInstanceOf[VerifierNodes[D]]) orElse
          treeTraverser(int.right.asInstanceOf[VerifierNodes[D]])
    }

    topNode.flatMap(t => treeTraverser(t))
  }
}
