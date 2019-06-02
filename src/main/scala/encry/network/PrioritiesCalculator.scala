package encry.network

import java.net.InetAddress

import com.typesafe.scalalogging.StrictLogging
import encry.network.PrioritiesCalculator.PeersPriorityStatus
import encry.network.PrioritiesCalculator.PeersPriorityStatus._
import encry.settings.EncryAppSettings
import scala.concurrent.duration._

final class PrioritiesCalculator(settings: EncryAppSettings) extends StrictLogging {

  private var peersNetworkStatistic: Map[InetAddress, (Requested, Received)] = Map.empty

  val updatingStatisticTime: FiniteDuration = (settings.network.deliveryTimeout._1 * settings.network.maxDeliveryChecks).seconds

  def incrementRequest(peer: InetAddress): Unit = {
    val (requested, received): (Requested, Received) = peersNetworkStatistic.getOrElse(peer, (Requested(), Received()))
    val newRequested: Requested = Requested(requested.increment)
    logger.info(s"Updating request parameter from $peer. Old is ($requested, $received). New one is: ($newRequested, $received)")
    peersNetworkStatistic = peersNetworkStatistic.updated(peer, (newRequested, received))
  }

  def incrementReceive(peer: InetAddress): Unit = {
    val (requested, received): (Requested, Received) = peersNetworkStatistic.getOrElse(peer, (Requested(), Received()))
    val newReceived: Received = Received(received.increment)
    logger.info(s"Updating received parameter from $peer. Old is ($requested, $received). New one is: ($requested, $newReceived)")
    peersNetworkStatistic = peersNetworkStatistic.updated(peer, (requested, newReceived))
  }

  def decrementRequest(peer: InetAddress): Unit = {
    val (requested, received): (Requested, Received) = peersNetworkStatistic.getOrElse(peer, (Requested(), Received()))
    val newRequested: Requested = Requested(requested.decrement)
    logger.info(s"Decrement request parameter from $peer. Old is ($requested, $received). New one is: ($newRequested, $received)")
    peersNetworkStatistic = peersNetworkStatistic.updated(peer, (newRequested, received))
  }

  def incrementRequestForNModifiers(peer: InetAddress, modifiersQty: Int): Unit = {
    val (requested, received): (Requested, Received) = peersNetworkStatistic.getOrElse(peer, (Requested(), Received()))
    val newRequested: Requested = Requested(requested.increment(modifiersQty))
    logger.info(s"Updating request parameter from $peer. Old is ($requested, $received). New one is: ($newRequested, $received)")
    peersNetworkStatistic = peersNetworkStatistic.updated(peer, (newRequested, received))
  }

  def accumulatePeersStatistic: Map[InetAddress, PeersPriorityStatus] = {
    val updatedStatistic: Map[InetAddress, PeersPriorityStatus] = peersNetworkStatistic.map {
      case (peer, (requested, received)) =>
        val priority: PeersPriorityStatus = PeersPriorityStatus.calculateStatuses(received, requested)
        peer -> priority
    }
    peersNetworkStatistic = Map.empty[InetAddress, (Requested, Received)]
    updatedStatistic
  }
}

object PrioritiesCalculator {

  final case class AccumulatedPeersStatistic(statistic: Map[InetAddress, PeersPriorityStatus]) extends AnyVal

  object PeersPriorityStatus {

    sealed trait PeersPriorityStatus

    final case class HighPriority(priority: Int = 4) extends AnyVal with PeersPriorityStatus {
      override def toString: String = "Priority status is: HighPriority"
    }

    final case class LowPriority(priority: Int = 3) extends AnyVal with PeersPriorityStatus {
      override def toString: String = "Priority status is: LowPriority"
    }

    final case class InitialPriority(priority: Int = 2) extends AnyVal with PeersPriorityStatus {
      override def toString: String = "Priority status is: InitialPriority"
    }

    final case class BadNode(priority: Int = 1) extends AnyVal with PeersPriorityStatus {
      override def toString: String = "Priority status is: BadNodePriority"
    }

    final case class Received(received: Int = 0) extends AnyVal {
      def increment: Int = received + 1

      override def toString: String = s"Received: $received"
    }

    final case class Requested(requested: Int = 0) extends AnyVal {
      def increment: Int = requested + 1

      def decrement: Int = requested - 1

      def increment(qty: Int): Int = requested + qty

      override def toString: String = s"Requested: $requested"
    }

    private val criterionForHighP: Double = 0.75
    private val criterionForLowP: Double = 0.50

    def calculateStatuses(res: Received, req: Requested): PeersPriorityStatus =
      res.received.toDouble / req.requested match {
        case t if t >= criterionForHighP => HighPriority()
        case t if t >= criterionForLowP => LowPriority()
        case _ => BadNode()
      }
  }
}