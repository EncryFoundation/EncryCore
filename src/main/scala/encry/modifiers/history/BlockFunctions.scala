package encry.modifiers.history

import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.Block
import org.encryfoundation.common.modifiers.mempool.directive.TransferDirective
import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
import org.encryfoundation.common.validation.{ModifierValidator, ValidationResult}
import scorex.crypto.encode.Base16

import scala.util.Try

object BlockFunctions {

  def toSeq(block: Block): Seq[PersistentModifier] = Seq(block.header, block.payload) ++ block.adProofsOpt.toSeq

  def transactions(block: Block): Seq[Transaction] = block.payload.txs

  def semanticValidity(block: Block): Try[Unit] = validateSemantically(block).toTry

  def validateSemantically(block: Block): ValidationResult = ModifierValidator.accumulateErrors
    .demand(block.header.transactionsRoot != block.payload.digest, "Invalid payload root hash")
    .result

  def dataString(block: Block): String = {
    val encodedId: String = Base16.encode(block.id)
    val encodedParentId: String = Base16.encode(block.parentId)
    val proofsRoot: String = Base16.encode(block.header.adProofsRoot)
    val stateRoot: String = Base16.encode(block.header.stateRoot)
    val transactionsRoot: String = Base16.encode(block.header.transactionsRoot)
    val proofs: String = block.adProofsOpt.map(p => Base16.encode(p.bytes)).getOrElse("")
    val solution: String = block.header.equihashSolution.ints.mkString("{", ", ", "}")
    val (minerAddress: String, minerReward: Long) = block.payload.txs.last.directives.head match {
      case TransferDirective(address, amount, tokenIdOpt) if tokenIdOpt.isEmpty => address -> amount
      case _ => "unknown" -> 0
    }
    val feesTotal: Long = block.payload.txs.map(_.fee).sum
    val txsSize: Int = block.payload.txs.map(_.bytes.length).sum

    s"('$encodedId', '$encodedParentId', '${block.header.version}', '${block.header.height}', '$proofsRoot'," +
      s" '$stateRoot', '$transactionsRoot', '${block.header.timestamp}', '${block.header.difficulty}'," +
      s" '${block.bytes.length}', '$solution', '$proofs', '${block.payload.txs.size}', '$minerAddress'," +
      s" '$minerReward', '$feesTotal', '$txsSize', TRUE)"
  }
}