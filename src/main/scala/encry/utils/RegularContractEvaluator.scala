package encry.utils

import encry.modifiers.state.Context
import org.encryfoundation.common.modifiers.mempool.transaction._
import org.encryfoundation.prismlang.core.wrapped.BoxedValue.Signature25519Value
import scorex.crypto.signatures.{Curve25519, Signature}

object RegularContractEvaluator {

  def eval(contract: RegularContract, ctx: Context, proofs: Seq[Proof]): Boolean = {
    contract match {
      case heightLockedContract: HeightLockedContract =>
        ctx.state.height > heightLockedContract.height
      case pubKeyLockedContract: PubKeyLockedContract =>
        Curve25519.verify(
          Signature @@ proofs.head.value.asInstanceOf[Signature25519Value].value.toArray,
          ctx.transaction.messageToSign,
          pubKeyLockedContract.pubKey
        )
      case _: OpenContract.type => true
      case _ => false
    }
  }
}
