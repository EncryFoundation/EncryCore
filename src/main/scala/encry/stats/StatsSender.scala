package encry.stats

import java.io.File
import java.net.InetAddress
import java.util
import java.text.SimpleDateFormat
import akka.actor.Actor
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.EncryApp.timeProvider
import encry.consensus.EncrySupplyController
import encry.modifiers.history.Header
import encry.settings.EncryAppSettings
import encry.stats.StatsSender._
import encry.stats.LoggingActor.LogMessage
import encry.view.history.History.Height
import org.encryfoundation.common.Algos
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

class StatsSender(settings: EncryAppSettings) extends Actor {

  var modifiersToDownload: Map[String, (ModifierTypeId, Long)] = Map()

  val nodeName: String = settings.network.nodeName match {
    case Some(value) => value
    case None => InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort
  }
  val InfluxURL: String = settings.influxDB.map(_.url).getOrElse(throw new RuntimeException("url not specified"))

  val InfluxLogin: String = settings.influxDB.map(_.login).getOrElse(throw new RuntimeException("login not specified"))

  val InfluxPassword: String = settings.influxDB.map(_.password).getOrElse(throw new RuntimeException("password not specified"))

  val InfluxPort: Int = settings.influxDB.map(_.udpPort).getOrElse(throw new RuntimeException("udp port not specified"))

  val influxDB: InfluxDB =
    InfluxDBFactory.connect(InfluxURL, InfluxLogin, InfluxPassword)

  influxDB.setRetentionPolicy("autogen")

  val modifiersToApply: mutable.Map[String, (ModifierTypeId, Long)] = mutable.Map[String, (ModifierTypeId, Long)]()

  override def preStart(): Unit =
    influxDB.write(InfluxPort, s"""nodesStartTime value="$nodeName"""")

  val sdf: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  override def receive: Receive = {
    case LogMessage(logLevel, logMessage, logTime) => influxDB.write(InfluxPort,
      s"""logsFromNode,nodeName=${nodeName},logLevel=${
        logLevel match {
          case "Info" => 1
          case "Debug" => 2
          case "Warn" => 3
          case "Error" => 4
          case _ => 4
        }
      } value="[${sdf.format(logTime)}] $logMessage"""")
    case HeightStatistics(bestHeaderHeight, bestBlockHeight) =>
      influxDB.write(InfluxPort,
        s"chainStat,nodeName=${nodeName} value=$bestHeaderHeight,bestBlockHeight=$bestBlockHeight")
    case BestHeaderInChain(fb: Header, applyTime: Long) =>
      influxDB.write(InfluxPort, util.Arrays.asList(
        s"difficulty,nodeName=${nodeName} diff=${fb.difficulty.toString},height=${fb.height}",
        s"""height,nodeName=${nodeName} header="${Algos.encode(fb.id)}",height=${fb.height}""",
        s"stateWeight,nodeName=${nodeName},height=${fb.height} " +
          s"value=${new File("encry/data/state/").listFiles.foldLeft(0L)(_ + _.length())}",
        s"historyWeight,nodeName=${nodeName},height=${fb.height} " +
          s"value=${new File("encry/data/history/").listFiles.foldLeft(0L)(_ + _.length())}",
        s"supply,nodeName=${nodeName},height=${fb.height} " +
          s"value=${EncrySupplyController.supplyAt(fb.height.asInstanceOf[Height])}"
      )
      )

    case MiningEnd(blockHeader: Header, workerIdx: Int, workersQty: Int) =>
      timeProvider
        .time()
        .map { time =>
          influxDB.write(
            InfluxPort,
            util.Arrays.asList(
              s"miningEnd,nodeName=${nodeName},block=${Algos.encode(blockHeader.id)}," +
                s"height=${blockHeader.height},worker=$workerIdx value=${time - blockHeader.timestamp}",
              s"minerIterCount,nodeName=${nodeName},block=${Algos.encode(blockHeader.id)}," +
                s"height=${blockHeader.height} value=${blockHeader.nonce - Long.MaxValue / workersQty * workerIdx + 1}"
            )
          )
        }

    case StartApplyingModif(modifierId: ModifierId, modifierTypeId: ModifierTypeId, startTime: Long) =>
      modifiersToApply += Algos.encode(modifierId) -> (modifierTypeId, startTime)

    case EndOfApplyingModif(modifierId) =>
      modifiersToApply.get(Algos.encode(modifierId)).foreach { modInfo =>
        influxDB.write(InfluxPort, s"modifApplying,nodeName=${nodeName}," +
          s"modType=${modInfo._1} value=${System.currentTimeMillis() - modInfo._2}")
        modifiersToApply -= Algos.encode(modifierId)
      }

    case TransactionGeneratorStat(txsQty: Int, generationTime: Long) =>
      influxDB.write(InfluxPort, s"transactionGenerator,nodeName=${nodeName} txsQty=$txsQty,generationTime=$generationTime")

    case SleepTime(time: Long) =>
      influxDB.write(InfluxPort, s"sleepTime,nodeName=${nodeName} value=$time")

    case MiningTime(time: Long) =>
      influxDB.write(InfluxPort, s"miningTime,nodeName=${nodeName} value=$time")

    case CandidateProducingTime(time: Long) =>
      influxDB.write(InfluxPort, s"candidateProducing,nodeName=${nodeName} value=$time")

    case StateUpdating(time: Long) =>
      influxDB.write(InfluxPort, s"stateUpdatingTime,nodeName=${nodeName} value=$time")

    case SendDownloadRequest(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId]) =>
      modifiersToDownload = modifiersToDownload ++ modifiers.map(mod => (Algos.encode(mod), (modifierTypeId, System.currentTimeMillis())))

    case GetModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId]) =>
      modifiers.foreach(downloadedModifierId =>
        modifiersToDownload.get(Algos.encode(downloadedModifierId)).foreach { dowloadInfo =>
          influxDB.write(
            InfluxPort,
            s"modDownloadStat,nodeName=${nodeName},modId=${Algos.encode(downloadedModifierId)}," +
              s"modType=${dowloadInfo._1} value=${System.currentTimeMillis() - dowloadInfo._2}"
          )
          modifiersToDownload = modifiersToDownload - Algos.encode(downloadedModifierId)
        }
      )
  }
}

object StatsSender {

  case class CandidateProducingTime(time: Long)

  case class SleepTime(time: Long)

  case class StartApplyingModif(modifierId: ModifierId, modifierTypeId: ModifierTypeId, startTime: Long)

  case class EndOfApplyingModif(modifierId: ModifierId)

  case class MiningEnd(blockHeader: Header, workerIdx: Int, workersQty: Int)

  case class MiningTime(time: Long)

  case class BestHeaderInChain(bestHeader: Header, applyTime: Long)

  case class SendDownloadRequest(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId])

  case class GetModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId])

  case class HeightStatistics(bestHeaderHeight: Int, bestBlockHeight: Int)

  case class StateUpdating(time: Long)

  case class TransactionGeneratorStat(txsQty: Int, generationTime: Long)

}
