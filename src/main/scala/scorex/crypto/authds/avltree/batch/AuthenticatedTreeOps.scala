package scorex.crypto.authds.avltree.batch

import scorex.crypto.authds.{Balance, _}
import scorex.crypto.hash.Digest
import scorex.utils.ByteArray
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

trait AuthenticatedTreeOps[D <: Digest] extends BatchProofConstants with ToStringHelper {

  type ChangeHappened = Boolean
  type HeightIncreased = Boolean
  type ToDelete = Boolean

  protected val keyLength: Int
  protected val valueLengthOpt: Option[Int]
  protected val collectChangedNodes: Boolean

  protected val changedNodesBuffer: ArrayBuffer[ProverNodes[D]] = ArrayBuffer.empty
  protected val changedNodesBufferToCheck: ArrayBuffer[ProverNodes[D]] = ArrayBuffer.empty

  protected val PositiveInfinityKey: ADKey = ADKey @@ Array.fill(keyLength)(-1: Byte)
  protected val NegativeInfinityKey: ADKey = ADKey @@ Array.fill(keyLength)(0: Byte)

  protected var rootNodeHeight: Int

  protected def onNodeVisit(n: Node[D], operation: Operation, isRotate: Boolean = false): Unit = {
    n match {
      case p: ProverNodes[D] if collectChangedNodes && !n.visited && !p.isNew =>
        if (isRotate) {
          changedNodesBufferToCheck += p
        } else if (operation.isInstanceOf[Insert] || operation.isInstanceOf[Remove]
          || operation.isInstanceOf[InsertOrUpdate]) {
          changedNodesBuffer += p
        } else if (!operation.isInstanceOf[Lookup]) {
          changedNodesBufferToCheck += p
        }
      case _ =>
    }
    n.visited = true
  }

  protected def digest(rootNode: Node[D]): ADDigest = {
    assert(rootNodeHeight >= 0 && rootNodeHeight < 256)
    ADDigest @@ (rootNode.label :+ rootNodeHeight.toByte)
  }

  protected def keyMatchesLeaf(key: ADKey, r: Leaf[D]): Boolean

  protected def nextDirectionIsLeft(key: ADKey, r: InternalNode[D]): Boolean

  protected def addNode(r: Leaf[D], key: ADKey, v: ADValue): InternalNode[D]

  protected def replayComparison: Int

  private def doubleLeftRotate(currentRoot: InternalNode[D], leftChild: Node[D], rightChild: InternalNode[D]): InternalNode[D] = {
    val newRoot = rightChild.left.asInstanceOf[InternalNode[D]]
    val (newLeftBalance: Balance, newRightBalance: Balance) = newRoot.balance match {
      case a if a == 0 =>
        (Balance @@ 0.toByte, Balance @@ 0.toByte)
      case a if a == -1 =>
        (Balance @@ 0.toByte, Balance @@ 1.toByte)
      case a if a == 1 =>
        (Balance @@ -1.toByte, Balance @@ 0.toByte)
    }
    val newLeftChild = currentRoot.getNew(newLeft = leftChild, newRight = newRoot.left, newBalance = newLeftBalance)
    val newRightChild = rightChild.getNew(newLeft = newRoot.right, newBalance = newRightBalance)
    newRoot.getNew(newLeft = newLeftChild, newRight = newRightChild, newBalance = Balance @@ 0.toByte)
  }

  private def doubleRightRotate(currentRoot: InternalNode[D], leftChild: InternalNode[D], rightChild: Node[D]): InternalNode[D] = {
    val newRoot = leftChild.right.asInstanceOf[InternalNode[D]]
    val (newLeftBalance: Balance, newRightBalance: Balance) = newRoot.balance match {
      case a if a == 0 =>
        (Balance @@ 0.toByte, Balance @@ 0.toByte)
      case a if a == -1 =>
        (Balance @@ 0.toByte, Balance @@ 1.toByte)
      case a if a == 1 =>
        (Balance @@ -1.toByte, Balance @@ 0.toByte)
    }
    val newRightChild = currentRoot.getNew(newRight = rightChild, newLeft = newRoot.right, newBalance = newRightBalance)
    val newLeftChild = leftChild.getNew(newRight = newRoot.left, newBalance = newLeftBalance)
    newRoot.getNew(newLeft = newLeftChild, newRight = newRightChild, newBalance = Balance @@ 0.toByte)
  }

  protected def returnResultOfOneOperation(operation: Operation, rootNode: Node[D]): Try[(Node[D], Option[ADValue])] = Try {
    val key = operation.key

    require(ByteArray.compare(key, NegativeInfinityKey) > 0, s"Key ${encoder.encode(key)} is less than -inf")
    require(ByteArray.compare(key, PositiveInfinityKey) < 0, s"Key ${encoder.encode(key)} is more than +inf")
    require(key.length == keyLength)

    var savedNode: Option[Leaf[D]] = None // The leaf to be saved in the hard deletion case, where we delete a leaf and copy its info over to another leaf

    def modifyHelper(rNode: Node[D], key: ADKey, operation: Operation): (Node[D], ChangeHappened, HeightIncreased, ToDelete, Option[ADValue]) = {
      rNode match {
        case r: Leaf[D] =>
          if (keyMatchesLeaf(key, r)) {
            operation match {
              case m: Modification =>
                m.updateFn(Some(r.value)) match {
                  case Success(None) => // delete key
                    onNodeVisit(r, operation)
                    (r, false, false, true, Some(r.value))
                  case Success(Some(v)) => // update value
                    valueLengthOpt.foreach(vl => require(v.length == vl, s"Value length is fixed and should be $vl"))
                    val oldValue = Some(r.value)
                    val rNew = r.getNew(newValue = v)
                    onNodeVisit(r, operation)
                    (rNew, true, false, false, oldValue)
                  case Failure(e) => // updateFunction doesn't like the value we found
                    throw e
                }
              case _: Lookup =>
                onNodeVisit(r, operation)
                (r, false, false, false, Some(r.value))
            }
          } else {
            // x > r.key
            operation match {
              case m: Modification =>
                m.updateFn(None) match {
                  case Success(None) => // don't change anything, just lookup
                    onNodeVisit(rNode, operation)
                    (r, false, false, false, None)
                  case Success(Some(v)) => // insert new value
                    valueLengthOpt.foreach(vl => require(v.length == vl, s"Value length is fixed and should be $vl"))
                    onNodeVisit(rNode, operation)
                    (addNode(r, key, v), true, true, false, None)
                  case Failure(e) => // updateFunctions doesn't like that we found nothing
                    throw e
                }
              case _: Lookup =>
                onNodeVisit(rNode, operation)
                (r, false, false, false, None)
            }
          }
        case r: InternalNode[D] =>
          if (nextDirectionIsLeft(key, r)) {
            val (newLeftM, changeHappened, childHeightIncreased, toDelete, oldValue) = modifyHelper(r.left, key, operation)
            onNodeVisit(r, operation)

            if (changeHappened) {
              if (childHeightIncreased && r.balance < 0) {
                val newLeft = newLeftM.asInstanceOf[InternalNode[D]]
                if (newLeft.balance < 0) {
                  val newR = r.getNew(newLeft = newLeft.right, newBalance = Balance @@ 0.toByte)
                  (newLeft.getNew(newRight = newR, newBalance = Balance @@ 0.toByte), true, false, false, oldValue)
                } else {
                  (doubleRightRotate(r, newLeft, r.right), true, false, false, oldValue)
                }
              } else {
                val myHeightIncreased = childHeightIncreased && r.balance == (0: Byte)
                val rBalance = if (childHeightIncreased) Balance @@ (r.balance - 1).toByte else r.balance
                (r.getNew(newLeft = newLeftM, newBalance = rBalance), true, myHeightIncreased, false, oldValue)
              }

            } else {
              (r, false, false, toDelete, oldValue)
            }
          } else {
            val (newRightM, changeHappened, childHeightIncreased, toDelete, oldValue) = modifyHelper(r.right, key, operation)
            onNodeVisit(r, operation)

            if (changeHappened) {
              if (childHeightIncreased && r.balance > 0) {
                val newRight = newRightM.asInstanceOf[InternalNode[D]]

                if (newRight.balance > 0) {
                  val newR = r.getNew(newRight = newRight.left, newBalance = Balance @@ 0.toByte)
                  (newRight.getNew(newLeft = newR, newBalance = Balance @@ 0.toByte), true, false, false, oldValue)
                } else {
                  (doubleLeftRotate(r, r.left, newRight), true, false, false, oldValue)
                }
              } else {
                val myHeightIncreased: Boolean = childHeightIncreased && r.balance == (0: Byte)
                val rBalance = if (childHeightIncreased) Balance @@ (r.balance + 1).toByte else r.balance
                (r.getNew(newRight = newRightM, newBalance = rBalance), true, myHeightIncreased, false, oldValue)
              }
            } else {
              (r, false, false, toDelete, oldValue)
            }
          }
        case _: LabelOnlyNode[D] =>
          throw new Error("Should never reach this point. If in prover, this is a bug. If in verifier, this proof is wrong.")
      }
    }

    def deleteHelper(r: InternalNode[D], deleteMax: Boolean): (Node[D], Boolean) = {

      def changeNextLeafKeyOfMaxNode(rNode: Node[D], nextLeafKey: ADKey): Node[D] = {
        onNodeVisit(rNode, operation)
        rNode match {
          case leaf: Leaf[D] =>
            leaf.getNew(newNextLeafKey = nextLeafKey)
          case rN: InternalNode[D] =>
            rN.getNew(newRight = changeNextLeafKeyOfMaxNode(rN.right, nextLeafKey))
          case _: LabelOnlyNode[D] =>
            throw new Error("Should never reach this point. If in prover, this is a bug. In in verifier, this proof is wrong.")
        }
      }

      def changeKeyAndValueOfMinNode(rNode: Node[D], newKey: ADKey, newValue: ADValue): Node[D] = {
        onNodeVisit(rNode, operation)
        rNode match {
          case leaf: Leaf[D] =>
            leaf.getNew(newKey = newKey, newValue = newValue)
          case rN: InternalNode[D] =>
            rN.getNew(newLeft = changeKeyAndValueOfMinNode(rN.left, newKey, newValue))
          case _: LabelOnlyNode[D] =>
            throw new Error("Should never reach this point. If in prover, this is a bug. If in verifier, this proof is wrong.")
        }
      }

      onNodeVisit(r, operation)

      val direction = if (deleteMax) 1 else replayComparison

      assert(!(direction < 0 && r.left.isInstanceOf[Leaf[D]]))

      if (direction >= 0 && r.right.isInstanceOf[Leaf[D]]) {

        val rightChild = r.right.asInstanceOf[Leaf[D]]
        onNodeVisit(rightChild, operation)
        if (deleteMax) {

          savedNode = Some(rightChild)
          (r.left, true)
        } else {

          assert(direction == 0)
          (changeNextLeafKeyOfMaxNode(r.left, rightChild.nextLeafKey), true)
        }
      } else if (direction == 0 && r.left.isInstanceOf[Leaf[D]]) {
        val leftChild = r.left.asInstanceOf[Leaf[D]]
        onNodeVisit(leftChild, operation)
        (changeKeyAndValueOfMinNode(r.right, leftChild.key, leftChild.value), true)
      } else {
        if (direction <= 0) {
          val (newLeft, childHeightDecreased) = deleteHelper(r.left.asInstanceOf[InternalNode[D]], direction == 0)

          val newRoot = if (direction == 0) {
            val s = savedNode.get
            savedNode = None
            val rWithChangedKey = r.getNewKey(s.key)
            rWithChangedKey.getNew(newRight = changeKeyAndValueOfMinNode(rWithChangedKey.right, s.key, s.value))
          } else {
            r
          }

          if (childHeightDecreased && newRoot.balance > 0) {
            onNodeVisit(newRoot.right, operation, isRotate = true)

            val rightChild = newRoot.right.asInstanceOf[InternalNode[D]]
            if (rightChild.balance < 0) {
              onNodeVisit(rightChild.left, operation, isRotate = true)
              (doubleLeftRotate(newRoot, newLeft, rightChild), true)
            } else {
              val newLeftChild = newRoot.getNew(newLeft = newLeft, newRight = rightChild.left,
                newBalance = Balance @@ (1 - rightChild.balance).toByte)
              val newR = rightChild.getNew(newLeft = newLeftChild,
                newBalance = Balance @@ (rightChild.balance - 1).toByte)
              (newR, newR.balance == 0)
            }
          } else {
            val newBalance = if (childHeightDecreased) Balance @@ (newRoot.balance + 1).toByte else newRoot.balance
            (newRoot.getNew(newLeft = newLeft, newBalance = newBalance), childHeightDecreased && newBalance == 0)
          }
        } else {
          val (newRight, childHeightDecreased) = deleteHelper(r.right.asInstanceOf[InternalNode[D]], deleteMax)
          if (childHeightDecreased && r.balance < 0) {
            onNodeVisit(r.left, operation, isRotate = true)

            val leftChild = r.left.asInstanceOf[InternalNode[D]]
            if (leftChild.balance > 0) {
              onNodeVisit(leftChild.right, operation, isRotate = true)
              (doubleRightRotate(r, leftChild, newRight), true)
            } else {
              val newRightChild = r.getNew(newLeft = leftChild.right, newRight = newRight,
                newBalance = Balance @@ (-leftChild.balance - 1).toByte)
              val newR = leftChild.getNew(newRight = newRightChild,
                newBalance = Balance @@ (1 + leftChild.balance).toByte)
              (newR, newR.balance == 0)
            }
          } else {
            val newBalance = if (childHeightDecreased) Balance @@ (r.balance - 1).toByte else r.balance
            (r.getNew(newRight = newRight, newBalance = newBalance), childHeightDecreased && newBalance == 0)
          }
        }
      }
    }

    val (newRootNode, _, heightIncreased, toDelete, oldValue) = modifyHelper(rootNode, key, operation)
    if (toDelete) {
      val (postDeleteRootNode, heightDecreased) = deleteHelper(newRootNode.asInstanceOf[InternalNode[D]], deleteMax = false)
      if (heightDecreased) rootNodeHeight -= 1
      (postDeleteRootNode, oldValue)
    } else {
      if (heightIncreased) rootNodeHeight += 1
      (newRootNode, oldValue)
    }
  }
}