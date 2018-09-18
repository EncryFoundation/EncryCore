package encry.avltree

import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ADKey, ADValue, Balance}
import scorex.crypto.encode.Base16
import scorex.crypto.hash.Digest
import scorex.utils.ByteArray
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

trait AuthenticatedTreeOps[D <: Digest] {

  protected def arrayToString(a: Array[Byte]): String = Base16.encode(a).take(8)

  // Do not use bytes -1, 0, or 1 -- these are for balance
  val LeafInPackagedProof: Byte = 2
  val LabelInPackagedProof: Byte = 3
  val EndOfTreeInPackagedProof: Byte = 4
  protected val keyLength: Int
  protected val valueLengthOpt: Option[Int]
  protected val collectChangedNodes: Boolean
  protected val changedNodesBuffer: ArrayBuffer[EncryProverNodes[D]] = ArrayBuffer.empty
  protected val changedNodesBufferToCheck: ArrayBuffer[EncryProverNodes[D]] = ArrayBuffer.empty
  protected val PositiveInfinityKey: ADKey = ADKey @@ Array.fill(keyLength)(-1: Byte)
  protected val NegativeInfinityKey: ADKey = ADKey @@ Array.fill(keyLength)(0: Byte)
  protected var rootNodeHeight: Int

  protected def onNodeVisit(n: EncryNode[D], operation: encry.avltree.Operation, isRotate: Boolean = false): Unit = {
    n match {
      case p: EncryProverNodes[D] if collectChangedNodes && !n.visited && !p.isNew =>
        if (isRotate) changedNodesBufferToCheck += p
        else if (operation.isInstanceOf[Insert] || operation.isInstanceOf[Remove] || operation.isInstanceOf[InsertOrUpdate])
          changedNodesBuffer += p
        else if (!operation.isInstanceOf[encry.avltree.Lookup]) changedNodesBufferToCheck += p
      case _ =>
    }
    n.visited = true
  }

  protected def digest(rootNode: EncryNode[D]): ADDigest = {
    assert(rootNodeHeight >= 0 && rootNodeHeight < 256)
    ADDigest @@ (rootNode.label :+ rootNodeHeight.toByte)
  }

  protected def keyMatchesLeaf(key: ADKey, r: EncryLeaf[D]): Boolean

  protected def nextDirectionIsLeft(key: ADKey, r: InternalEncryNode[D]): Boolean

  protected def addNode(r: EncryLeaf[D], key: ADKey, v: ADValue): InternalEncryNode[D]

  protected def replayComparison: Int

  private def doubleLeftRotate(currentRoot: InternalEncryNode[D], leftChild: EncryNode[D],
                               rightChild: InternalEncryNode[D]): InternalEncryNode[D] = {
    val newRoot: InternalEncryNode[D] = rightChild.left.asInstanceOf[InternalEncryNode[D]]
    val (newLeftBalance: Balance, newRightBalance: Balance) = newRoot.balance match {
      case a if a == 0 => (Balance @@ 0.toByte, Balance @@ 0.toByte)
      case a if a == -1 => (Balance @@ 0.toByte, Balance @@ 1.toByte)
      case a if a == 1 => (Balance @@ -1.toByte, Balance @@ 0.toByte)
    }
    val newLeftChild: InternalEncryNode[D] = currentRoot
      .getNew(newLeft = leftChild, newRight = newRoot.left, newBalance = newLeftBalance)
    val newRightChild: InternalEncryNode[D] = rightChild.getNew(newLeft = newRoot.right, newBalance = newRightBalance)
    newRoot.getNew(newLeft = newLeftChild, newRight = newRightChild, newBalance = Balance @@ 0.toByte)
  }

  private def doubleRightRotate(currentRoot: InternalEncryNode[D], leftChild: InternalEncryNode[D],
                                rightChild: EncryNode[D]): InternalEncryNode[D] = {
    val newRoot: InternalEncryNode[D] = leftChild.right.asInstanceOf[InternalEncryNode[D]]
    val (newLeftBalance: Balance, newRightBalance: Balance) = newRoot.balance match {
      case a if a == 0 => (Balance @@ 0.toByte, Balance @@ 0.toByte)
      case a if a == -1 => (Balance @@ 0.toByte, Balance @@ 1.toByte)
      case a if a == 1 => (Balance @@ -1.toByte, Balance @@ 0.toByte)
    }
    val newRightChild: InternalEncryNode[D] = currentRoot
      .getNew(newRight = rightChild, newLeft = newRoot.right, newBalance = newRightBalance)
    val newLeftChild: InternalEncryNode[D] = leftChild.getNew(newRight = newRoot.left, newBalance = newLeftBalance)
    newRoot.getNew(newLeft = newLeftChild, newRight = newRightChild, newBalance = Balance @@ 0.toByte)
  }

  protected def returnResultOfOneOperation(operation: encry.avltree.Operation,
                                           rootNode: EncryNode[D]): Try[(EncryNode[D], Option[ADValue])] = Try {
    val key: ADKey = operation.key

    require(ByteArray.compare(key, NegativeInfinityKey) > 0, s"Key ${Base16.encode(key)} is less than -inf")
    require(ByteArray.compare(key, PositiveInfinityKey) < 0, s"Key ${Base16.encode(key)} is more than +inf")
    require(key.length == keyLength)

    var savedNode: Option[EncryLeaf[D]] = None

    def modifyHelper(rNode: EncryNode[D], key: ADKey,
                     operation: encry.avltree.Operation): (EncryNode[D], Boolean, Boolean, Boolean, Option[ADValue]) = {
      rNode match {
        case r: EncryLeaf[D] =>
          if (keyMatchesLeaf(key, r)) operation match {
            case m: encry.avltree.Modification => m.updateFn(Some(r.value)) match {
              case Success(None) => // delete key
                onNodeVisit(r, operation)
                (r, false, false, true, Some(r.value))
              case Success(Some(v)) => // update value
                valueLengthOpt.foreach(vl => require(v.length == vl, s"Value length is fixed and should be $vl"))
                val oldValue: Option[ADValue] = Some(r.value)
                val rNew: EncryLeaf[D] = r.getNew(newValue = v)
                onNodeVisit(r, operation)
                (rNew, true, false, false, oldValue)
              case Failure(e) => throw e // updateFunction doesn't like the value we found
            }
            case _: Lookup =>
              onNodeVisit(r, operation)
              (r, false, false, false, Some(r.value))
          } else operation match {
            case m: Modification => m.updateFn(None) match {
              case Success(None) => // don't change anything, just lookup
                onNodeVisit(rNode, operation)
                (r, false, false, false, None)
              case Success(Some(v)) => // insert new value
                valueLengthOpt.foreach(vl => require(v.length == vl, s"Value length is fixed and should be $vl"))
                onNodeVisit(rNode, operation)
                (addNode(r, key, v), true, true, false, None)
              case Failure(e) => throw e // updateFunctions doesn't like that we found nothing
            }
            case _: Lookup =>
              onNodeVisit(rNode, operation)
              (r, false, false, false, None)
          }
        case r: InternalEncryNode[D] =>
          if (nextDirectionIsLeft(key, r)) {
            val (newLeftM, changeHappened, childHeightIncreased, toDelete, oldValue) = modifyHelper(r.left, key, operation)
            onNodeVisit(r, operation)
            if (changeHappened) {
              if (childHeightIncreased && r.balance < 0) {
                val newLeft: InternalEncryNode[D] = newLeftM.asInstanceOf[InternalEncryNode[D]]
                if (newLeft.balance < 0) {
                  val newR: InternalEncryNode[D] = r.getNew(newLeft = newLeft.right, newBalance = Balance @@ 0.toByte)
                  (newLeft.getNew(newRight = newR, newBalance = Balance @@ 0.toByte), true, false, false, oldValue)
                } else (doubleRightRotate(r, newLeft, r.right), true, false, false, oldValue)
              } else {
                val myHeightIncreased: Boolean = childHeightIncreased && r.balance == (0: Byte)
                val rBalance: Balance = if (childHeightIncreased) Balance @@ (r.balance - 1).toByte else r.balance
                (r.getNew(newLeft = newLeftM, newBalance = rBalance), true, myHeightIncreased, false, oldValue)
              }
            } else (r, false, false, toDelete, oldValue)
          } else {
            val (newRightM, changeHappened, childHeightIncreased, toDelete, oldValue) = modifyHelper(r.right, key, operation)
            onNodeVisit(r, operation)
            if (changeHappened) {
              if (childHeightIncreased && r.balance > 0) {
                val newRight: InternalEncryNode[D] = newRightM.asInstanceOf[InternalEncryNode[D]]
                if (newRight.balance > 0) {
                  val newR: InternalEncryNode[D] = r.getNew(newRight = newRight.left, newBalance = Balance @@ 0.toByte)
                  (newRight.getNew(newLeft = newR, newBalance = Balance @@ 0.toByte), true, false, false, oldValue)
                } else (doubleLeftRotate(r, r.left, newRight), true, false, false, oldValue)
              } else {
                val myHeightIncreased: Boolean = childHeightIncreased && r.balance == (0: Byte)
                val rBalance: Balance = if (childHeightIncreased) Balance @@ (r.balance + 1).toByte else r.balance
                (r.getNew(newRight = newRightM, newBalance = rBalance), true, myHeightIncreased, false, oldValue)
              }
            } else (r, false, false, toDelete, oldValue)
          }
        case _: LabelOnlyEncryNode[D] =>
          throw new Error("Should never reach this point. If in prover, this is a bug. If in verifier, this proof is wrong.")
      }
    }

    def deleteHelper(r: InternalEncryNode[D], deleteMax: Boolean): (EncryNode[D], Boolean) = {

      def changeNextLeafKeyOfMaxNode(rNode: EncryNode[D], nextLeafKey: ADKey): EncryNode[D] = {
        onNodeVisit(rNode, operation)
        rNode match {
          case leaf: EncryLeaf[D] => leaf.getNew(newNextLeafKey = nextLeafKey)
          case rN: InternalEncryNode[D] => rN.getNew(newRight = changeNextLeafKeyOfMaxNode(rN.right, nextLeafKey))
          case _: LabelOnlyEncryNode[D] =>
            throw new Error("Should never reach this point. If in prover, this is a bug. In in verifier, this proof is wrong.")
        }
      }

      def changeKeyAndValueOfMinNode(rNode: EncryNode[D], newKey: ADKey, newValue: ADValue): EncryNode[D] = {
        onNodeVisit(rNode, operation)
        rNode match {
          case leaf: EncryLeaf[D] => leaf.getNew(newKey = newKey, newValue = newValue)
          case rN: InternalEncryNode[D] => rN.getNew(newLeft = changeKeyAndValueOfMinNode(rN.left, newKey, newValue))
          case _: LabelOnlyEncryNode[D] =>
            throw new Error("Should never reach this point. If in prover, this is a bug. If in verifier, this proof is wrong.")
        }
      }

      onNodeVisit(r, operation)

      val direction: Int = if (deleteMax) 1 else replayComparison

      assert(!(direction < 0 && r.left.isInstanceOf[EncryLeaf[D]]))

      if (direction >= 0 && r.right.isInstanceOf[EncryLeaf[D]]) {
        val rightChild: EncryLeaf[D] = r.right.asInstanceOf[EncryLeaf[D]]
        onNodeVisit(rightChild, operation)
        if (deleteMax) {
          savedNode = Some(rightChild)
          (r.left, true)
        } else {
          assert(direction == 0)
          (changeNextLeafKeyOfMaxNode(r.left, rightChild.nextLeafKey), true)
        }
      } else if (direction == 0 && r.left.isInstanceOf[EncryLeaf[D]]) {
        val leftChild: EncryLeaf[D] = r.left.asInstanceOf[EncryLeaf[D]]
        onNodeVisit(leftChild, operation)
        (changeKeyAndValueOfMinNode(r.right, leftChild.key, leftChild.value), true)
      } else {
        if (direction <= 0) {
          val (newLeft: EncryNode[D], childHeightDecreased: Boolean) =
            deleteHelper(r.left.asInstanceOf[InternalEncryNode[D]], direction == 0)

          val newRoot: InternalEncryNode[D] = if (direction == 0) {
            val s: EncryLeaf[D] = savedNode.get
            savedNode = None
            val rWithChangedKey: InternalEncryNode[D] = r.getNewKey(s.key)
            rWithChangedKey.getNew(newRight = changeKeyAndValueOfMinNode(rWithChangedKey.right, s.key, s.value))
          } else r

          if (childHeightDecreased && newRoot.balance > 0) {
            onNodeVisit(newRoot.right, operation, isRotate = true)

            val rightChild: InternalEncryNode[D] = newRoot.right.asInstanceOf[InternalEncryNode[D]]
            if (rightChild.balance < 0) {
              onNodeVisit(rightChild.left, operation, isRotate = true)
              (doubleLeftRotate(newRoot, newLeft, rightChild), true)
            } else {
              val newLeftChild: InternalEncryNode[D] = newRoot.getNew(newLeft = newLeft, newRight = rightChild.left,
                newBalance = Balance @@ (1 - rightChild.balance).toByte)
              val newR: InternalEncryNode[D] = rightChild.getNew(newLeft = newLeftChild,
                newBalance = Balance @@ (rightChild.balance - 1).toByte)
              (newR, newR.balance == 0)
            }
          } else {
            val newBalance: Balance = if (childHeightDecreased) Balance @@ (newRoot.balance + 1).toByte else newRoot.balance
            (newRoot.getNew(newLeft = newLeft, newBalance = newBalance), childHeightDecreased && newBalance == 0)
          }
        } else {
          val (newRight: EncryNode[D], childHeightDecreased: Boolean) =
            deleteHelper(r.right.asInstanceOf[InternalEncryNode[D]], deleteMax)
          if (childHeightDecreased && r.balance < 0) {
            onNodeVisit(r.left, operation, isRotate = true)
            val leftChild: InternalEncryNode[D] = r.left.asInstanceOf[InternalEncryNode[D]]
            if (leftChild.balance > 0) {
              onNodeVisit(leftChild.right, operation, isRotate = true)
              (doubleRightRotate(r, leftChild, newRight), true)
            } else {
              val newRightChild: InternalEncryNode[D] = r.getNew(newLeft = leftChild.right, newRight = newRight,
                newBalance = Balance @@ (-leftChild.balance - 1).toByte)
              val newR: InternalEncryNode[D] = leftChild.getNew(newRight = newRightChild,
                newBalance = Balance @@ (1 + leftChild.balance).toByte)
              (newR, newR.balance == 0)
            }
          } else {
            val newBalance: Balance = if (childHeightDecreased) Balance @@ (r.balance - 1).toByte else r.balance
            (r.getNew(newRight = newRight, newBalance = newBalance), childHeightDecreased && newBalance == 0)
          }
        }
      }
    }

    val (newRootNode: EncryNode[D], _, heightIncreased: Boolean, toDelete: Boolean, oldValue: Option[ADValue]) =
      modifyHelper(rootNode, key, operation)
    if (toDelete) {
      val (postDeleteRootNode: EncryNode[D], heightDecreased: Boolean) =
        deleteHelper(newRootNode.asInstanceOf[InternalEncryNode[D]], deleteMax = false)
      if (heightDecreased) rootNodeHeight -= 1
      (postDeleteRootNode, oldValue)
    } else {
      if (heightIncreased) rootNodeHeight += 1
      (newRootNode, oldValue)
    }
  }
}