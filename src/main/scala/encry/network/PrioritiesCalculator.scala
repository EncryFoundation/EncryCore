package encry.network

import java.net.InetSocketAddress
import com.typesafe.scalalogging.StrictLogging
import encry.network.PrioritiesCalculator.PeersPriorityStatus
import encry.network.PrioritiesCalculator.PeersPriorityStatus._
import encry.settings.EncryAppSettings
import scala.concurrent.duration._

final class PrioritiesCalculator(settings: EncryAppSettings) extends StrictLogging {

  private var peersNetworkStatistic: Map[InetSocketAddress, (Requested, Received)] = Map.empty

  val updatingStatisticTime: FiniteDuration = (settings.network.deliveryTimeout._1 * settings.network.maxDeliveryChecks).seconds

  def incrementRequest(peer: InetSocketAddress): Unit = {
    val (requested, received): (Requested, Received) = peersNetworkStatistic.getOrElse(peer, (Requested(), Received()))
    val newRequested: Requested = requested.increment
    logger.debug(s"Updating request parameter from $peer. Old is ($requested, $received). New one is: ($newRequested, $received)")
    peersNetworkStatistic = peersNetworkStatistic.updated(peer, (newRequested, received))
  }

  def incrementReceive(peer: InetSocketAddress): Unit = {
    val (requested, received): (Requested, Received) = peersNetworkStatistic.getOrElse(peer, (Requested(), Received()))
    val newReceived: Received = received.increment
    logger.debug(s"Updating received parameter from $peer. Old is ($requested, $received). New one is: ($requested, $newReceived)")
    peersNetworkStatistic = peersNetworkStatistic.updated(peer, (requested, newReceived))
  }

  def decrementRequest(peer: InetSocketAddress): Unit = {
    val (requested, received): (Requested, Received) = peersNetworkStatistic.getOrElse(peer, (Requested(), Received()))
    val newRequested: Requested = requested.decrement
    logger.debug(s"Decrement request parameter from $peer. Old is ($requested, $received). New one is: ($newRequested, $received)")
    peersNetworkStatistic = peersNetworkStatistic.updated(peer, (newRequested, received))
  }

  def incrementRequestForNModifiers(peer: InetSocketAddress, modifiersQty: Int): Unit = {
    val (requested, received): (Requested, Received) = peersNetworkStatistic.getOrElse(peer, (Requested(), Received()))
    val newRequested: Requested = requested.incrementForN(modifiersQty)
    logger.debug(s"Updating request parameter from $peer. Old is ($requested, $received). New one is: ($newRequested, $received)")
    peersNetworkStatistic = peersNetworkStatistic.updated(peer, (newRequested, received))
  }

  def accumulatePeersStatistic: Map[InetSocketAddress, PeersPriorityStatus] = {
    val updatedStatistic: Map[InetSocketAddress, PeersPriorityStatus] = peersNetworkStatistic.map {
      case (peer, (requested, received)) =>
        logger.info(s"peer: $peer: received: $received, requested: $requested")
        val priority: PeersPriorityStatus = PeersPriorityStatus.calculateStatuses(received, requested)
        peer -> priority
    }
    peersNetworkStatistic = Map.empty[InetSocketAddress, (Requested, Received)]
    logger.info(s"Accumulated peers statistic. Current stats are: ${updatedStatistic.mkString(",")}")
    updatedStatistic
  }
}

object PrioritiesCalculator {

  final case class AccumulatedPeersStatistic(statistic: Map[InetSocketAddress, PeersPriorityStatus])

  object PeersPriorityStatus {

    sealed trait PeersPriorityStatus

    case object HighPriority extends PeersPriorityStatus

    case object LowPriority extends PeersPriorityStatus

    case object InitialPriority extends PeersPriorityStatus

    case object BadNode extends PeersPriorityStatus

    final case class Received(received: Int = 0) extends AnyVal {
      def increment: Received = Received(received + 1)
    }

    final case class Requested(requested: Int = 0) extends AnyVal {
      def increment: Requested = Requested(requested + 1)

      def decrement: Requested = Requested(requested - 1)

      def incrementForN(n: Int): Requested = Requested(requested + n)
    }

    private val criterionForHighP: Double = 0.75
    private val criterionForLowP: Double  = 0.50

    def calculateStatuses(res: Received, req: Requested): PeersPriorityStatus =
      res.received.toDouble / req.requested match {
        case t if t >= criterionForHighP => HighPriority
        case t if t >= criterionForLowP  => LowPriority
        case _                           => BadNode
      }
  }
}