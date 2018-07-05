package encry.utils

import java.net.InetAddress

import encry.utils.NetworkTime.Time
import org.apache.commons.net.ntp.{NTPUDPClient, TimeInfo}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Left

object NetworkTime {
  def localWithOffset(offset: Long): Long = System.currentTimeMillis() + offset

  type Offset = Long
  type Time = Long
}

protected case class NetworkTime(offset: NetworkTime.Offset, lastUpdate: NetworkTime.Time)

case class NetworkTimeProviderSettings(server: String, updateEvery: FiniteDuration, timeout: FiniteDuration)

class NetworkTimeProvider(ntpSettings: NetworkTimeProviderSettings) extends Logging {

  private type State = Either[(NetworkTime, Future[NetworkTime]), NetworkTime]

  private def updateOffSet(): Option[NetworkTime.Offset] = {
    val client: NTPUDPClient = new NTPUDPClient()
    client.setDefaultTimeout(ntpSettings.timeout.toMillis.toInt)
    try {
      client.open()
      val info: TimeInfo = client.getTime(InetAddress.getByName(ntpSettings.server))
      info.computeDetails()
      Option(info.getOffset)
    } catch {
      case t: Throwable =>
        None
    } finally {
      client.close()
    }
  }

  private def timeAndState(currentState: State): (NetworkTime.Time, State) = {
    currentState match {
      case Right(nt) =>
        val time: Long = NetworkTime.localWithOffset(nt.offset)
        val state: Either[(NetworkTime, Future[NetworkTime]), NetworkTime] =
          if (time > nt.lastUpdate + ntpSettings.updateEvery.toMillis) {
            Left(nt -> Future(updateOffSet()).map { mbOffset =>
              log.info("New offset adjusted: " + mbOffset)
              val offset = mbOffset.getOrElse(nt.offset)
              NetworkTime(offset, NetworkTime.localWithOffset(offset))
            })
          } else Right(nt)
        (time, state)
      case Left((nt, f)) =>
        if (f.isCompleted) {
          val nnt = Await.result(f, 10.seconds)
          NetworkTime.localWithOffset(nnt.offset) -> Right(nnt)
        } else NetworkTime.localWithOffset(nt.offset) -> Left(nt -> f)
    }
  }

  private var state: State = Right(NetworkTime(0L, 0L))

  def time(): NetworkTime.Time = {
    val t: (Time, State) = timeAndState(state)
    state = t._2
    t._1
  }
}