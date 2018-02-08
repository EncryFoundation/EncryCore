package encry.modifiers.state.box

import com.google.common.primitives.Longs
import encry.modifiers.mempool.EncryTransaction
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.settings.Algos
import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Digest32

import scala.util.Try

// TODO: Should substitute `scorex.core.transaction.box.Box[P]` in the future.
trait EncryBaseBox extends Box[Proposition] with JsonSerializable {

  val typeId: BxTypeId

  val bxHash: Digest32

  val nonce: Long

  override lazy val id: ADKey = ADKey @@ Algos.hash(Longs.toByteArray(nonce)).updated(0, typeId) // 32 bytes!

  def unlockTry(modifier: EncryTransaction, script: Option[String], ctxOpt: Option[Context]): Try[Unit]

  override def toString: String = s"<Box type=:$typeId id=:${Algos.encode(id)}>"
}
