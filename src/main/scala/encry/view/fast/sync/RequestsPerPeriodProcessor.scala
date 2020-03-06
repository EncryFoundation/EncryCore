package encry.view.fast.sync

import com.typesafe.scalalogging.StrictLogging
import encry.network.PeerConnectionHandler.ConnectedPeer
import encry.settings.EncryAppSettings
import org.encryfoundation.common.utils.Algos

final case class RequestsPerPeriodProcessor(handledRequests: Int, settings: EncryAppSettings) extends StrictLogging {

  def canBeProcessed(processor: SnapshotHolder, manifestId: Array[Byte]): Boolean = {
    val actualManifestID: Option[Array[Byte]] = processor.actualManifestId
    logger.info(s"Requested id ${Algos.encode(manifestId)}, current manifest id ${actualManifestID.map(Algos.encode)}.")
    actualManifestID.exists(_.sameElements(manifestId))
  }

  def canProcessRequest(remote: ConnectedPeer): Boolean = handledRequests <= settings.snapshotSettings.requestsPerTime

  def processRequest(remote: ConnectedPeer): RequestsPerPeriodProcessor = this.copy(handledRequests + 1)

  def iterationProcessing: RequestsPerPeriodProcessor = this.copy(handledRequests = 0)

}

object RequestsPerPeriodProcessor {
  def empty(settings: EncryAppSettings): RequestsPerPeriodProcessor = RequestsPerPeriodProcessor(0, settings)
}
