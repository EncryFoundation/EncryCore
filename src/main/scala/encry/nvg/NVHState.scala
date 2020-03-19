package encry.nvg

import java.io.File

import akka.actor.{Actor, ActorRef, Props}
import encry.settings.{EncryAppSettings, NodeSettings}
import encry.view.NodeViewErrors.ModifierApplyError
import encry.view.state.UtxoState
import org.encryfoundation.common.utils.TaggedTypes.ModifierId

class NVHState(influxRef: Option[ActorRef]) extends Actor {
  override def receive: Receive = ???
}

object NVHState {

  sealed trait StateAction
  object StateAction {
    case class ModifierApplied(modifierId: ModifierId) extends StateAction
    case class Rollback(branchPoint: ModifierId) extends StateAction
    case class ValidationFailed(modifierId: ModifierId, errs: List[ModifierApplyError]) extends StateAction
  }

  def props(settings: EncryAppSettings, branchPoint: ModifierId, influxRef: Option[ActorRef]): Props = {
    val stateDir: File = UtxoState.getStateDir(settings)
    val rootsDir: File = UtxoState.getRootsDir(settings)
    val state: UtxoState = UtxoState.genesis(stateDir, rootsDir, settings, influxRef)
    Props(new NVHState(influxRef))
  }
  def props(settings: EncryAppSettings, influxRef: Option[ActorRef]): Props = Props(new NVHState(influxRef))
}
