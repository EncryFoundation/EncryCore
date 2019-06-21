package encry.view.state

import com.typesafe.scalalogging.StrictLogging
import encry.storage.VersionalStorage
import encry.utils.CoreTaggedTypes.VersionTag
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Block, Header}
import org.encryfoundation.common.modifiers.mempool.transaction.Input
import org.encryfoundation.common.modifiers.state.box.EncryBaseBox
import org.encryfoundation.common.utils.Algos
import scala.util.Try

final case class UtxoStateWithoutAVL(storage: VersionalStorage) extends MinimalState[PersistentModifier, UtxoStateWithoutAVL] with StrictLogging {

  override def applyModifier(mod: PersistentModifier): Try[UtxoStateWithoutAVL] = ???

//    mod match {
//    case header: Header =>
//    case block: Block =>
//      logger.info(s"\n\nStarting to applyModifier as a Block: ${Algos.encode(mod.id)} to state at height")
//      //val  = block.payload.txs
//      UtxoStateWithoutAVL()
//  }

  override def rollbackTo(version: VersionTag): Try[UtxoStateWithoutAVL] = ???

  override def version: VersionTag = ???

  override type NVCT = this.type
}

object UtxoStateWithoutAVL {

  final case class StateChange(inputs: IndexedSeq[Input],
                               outputs: List[EncryBaseBox])
}
