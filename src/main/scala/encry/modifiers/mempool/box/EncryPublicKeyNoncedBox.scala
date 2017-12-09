package encry.modifiers.mempool.box

import com.google.common.primitives.Longs
import encry.modifiers.mempool.box.body.BaseBoxBody
import scorex.core.ModifierId
import scorex.core.transaction.box.proposition.{Proposition, PublicKey25519Proposition}
import scorex.crypto.authds.ADKey
import scorex.crypto.hash.Blake2b256

trait EncryPublicKeyNoncedBox[P <: PublicKey25519Proposition, BB <: BaseBoxBody] extends EncryBaseBox[P, BB] {
  val nonce: Long

  override lazy val id: ADKey = ADKey @@ EncryPublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val publicKey: P = proposition

  override def equals(obj: Any): Boolean = obj match {
      // TODO: Implement `equals()` method in the `BoxBody`.
    case acc: EncryPublicKeyNoncedBox[P, BB] => (acc.id sameElements this.id) && acc.body == this.body
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}

object EncryPublicKeyNoncedBox {
  def idFromBox[P <: PublicKey25519Proposition](prop: P, nonce: Long): ModifierId =
    ModifierId @@ Blake2b256(prop.pubKeyBytes ++ Longs.toByteArray(nonce))
}
