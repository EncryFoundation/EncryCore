package encry.modifiers.state.box

import encry.modifiers.mempool.EncryTransaction
import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.box.Box.Amount
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.hash.Digest32

import scala.util.Try

trait EncryBox[P <: Proposition] extends EncryBaseBox with JsonSerializable {

  import EncryBox._

  override val proposition: P

  val typeId: BxTypeId

  val bxHash: Digest32

  def unlockTry(modifier: EncryTransaction, script: Option[String]): Try[Unit]

  // Shadow redundant field from base class.
  override val value: Amount = 0L
}

object EncryBox {

  type BxTypeId = Byte

  val BoxIdSize = 32
}
