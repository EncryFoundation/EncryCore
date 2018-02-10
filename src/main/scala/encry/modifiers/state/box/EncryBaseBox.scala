package encry.modifiers.state.box

import com.google.common.primitives.Longs
import encry.modifiers.mempool.EncryBaseTransaction
import encry.modifiers.state.box.EncryBox.BxTypeId
import encry.settings.Algos
import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds.ADKey

import scala.util.Try

trait EncryBaseBox extends Box[Proposition] with JsonSerializable {

  val typeId: BxTypeId

  val nonce: Long

  override lazy val id: ADKey = ADKey @@ Algos.hash(Longs.toByteArray(nonce)).updated(0, typeId) // 32 bytes!

  def unlockTry(modifier: EncryBaseTransaction, script: Option[String])(implicit ctxOpt: Option[Context]): Try[Unit]

  override def toString: String = s"<Box type=:$typeId id=:${Algos.encode(id)}>"
}
