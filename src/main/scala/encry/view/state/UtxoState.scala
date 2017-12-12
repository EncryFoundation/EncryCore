package encry.view.state

import akka.actor.ActorRef
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.EncryPaymentTransaction
import encry.modifiers.state.box.EncryPaymentBox
import encry.modifiers.state.box.body.PaymentBoxBody
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.VersionTag
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.TransactionValidation
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest

import scala.util.Try

class UtxoState(override val version: VersionTag,
                val storage: Store,
                nodeViewHolderRef: Option[ActorRef])
  extends EncryBaseState[PublicKey25519Proposition, PaymentBoxBody, EncryPaymentBox, EncryPaymentTransaction, UtxoState]
    with TransactionValidation[PublicKey25519Proposition, EncryPaymentTransaction] with ScorexLogging {

  // TODO: Why 10?
  override def maxRollbackDepth: Int = 10

  override def applyModifier(mod: EncryPersistentModifier): Try[UtxoState] = ???

  override def rollbackTo(version: VersionTag): Try[UtxoState] = ???

  override def rollbackVersions: Iterable[VersionTag] = ???

  override lazy val rootHash: ADDigest = ???

  override def validate(tx: EncryPaymentTransaction): Try[Unit] = ???

}
