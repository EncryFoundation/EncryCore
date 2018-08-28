package encry.stats

import java.io.File
import java.util
import java.text.SimpleDateFormat
import akka.actor.Actor
import encry.EncryApp.{settings, timeProvider}
import encry.consensus.EncrySupplyController
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.stats.StatsSender._
import encry.view.history
import encry.{ModifierId, ModifierTypeId}
import encry.stats.LoggingActor.LogMessage
import org.encryfoundation.common.Algos
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

class StatsSender extends Actor {

  var modifiersToDownload: Map[String, (ModifierTypeId, Long)] = Map()//todo delete after completed task about stat

  val influxDB: InfluxDB =
    InfluxDBFactory.connect(settings.influxDB.url, settings.influxDB.login, settings.influxDB.password)

  influxDB.setRetentionPolicy("autogen")

  val modifiersToApply: mutable.Map[String, (ModifierTypeId, Long)] = mutable.Map[String, (ModifierTypeId, Long)]()

  override def preStart(): Unit =
    influxDB.write(settings.influxDB.udpPort, s"""nodesStartTime value="${settings.network.nodeName}"""")

  val sdf: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  override def receive: Receive = {
    case LogMessage(logLevel, logMessage, logsTime) => influxDB.write(8089,
      s"""logsFromNode,nodeName=${settings.network.nodeName},logLevel=${
        logLevel match {
          case "Info" => 1
          case "Debug" => 2
          case "Warn" => 3
          case "Error" => 4
          case _ => 4
        }
      } value="[${sdf.format(logsTime)}], $logMessage"""")
    case BlocksStat(notCompletedBlocks: Int, headerCache: Int, payloadCache: Int, completedBlocks: Int) =>
      influxDB.write(settings.influxDB.udpPort, s"blocksStatistic headerStats=$headerCache,payloadStats=$payloadCache," +
        s"completedBlocksStat=$completedBlocks,notCompletedBlocksStat=$notCompletedBlocks")
    case BestHeaderInChain(fb: EncryBlockHeader) =>
      influxDB.write(settings.influxDB.udpPort, util.Arrays.asList(
        s"difficulty,nodeName=${settings.network.nodeName} diff=${fb.difficulty.toString},height=${fb.height}",
        s"height,nodeName=${settings.network.nodeName},header=${Algos.encode(fb.id)} height=${fb.height}",
        s"stateWeight,nodeName=${settings.network.nodeName},height=${fb.height} " +
          s"value=${new File("encry/data/state/").listFiles.foldLeft(0L)(_ + _.length())}",
        s"historyWeight,nodeName=${settings.network.nodeName},height=${fb.height} " +
          s"value=${new File("encry/data/history/").listFiles.foldLeft(0L)(_ + _.length())}",
        s"supply,nodeName=${settings.network.nodeName},height=${fb.height} " +
          s"value=${EncrySupplyController.supplyAt(fb.height.asInstanceOf[history.Height])}"
      )
      )

    case MiningEnd(blockHeader: EncryBlockHeader, workerIdx: Int, workersQty: Int) =>
      timeProvider
        .time()
        .map { time =>
          influxDB.write(
            settings.influxDB.udpPort,
            util.Arrays.asList(
              s"miningEnd,nodeName=${settings.network.nodeName},block=${Algos.encode(blockHeader.id)}," +
                s"height=${blockHeader.height},worker=$workerIdx value=${time - blockHeader.timestamp}",
              s"minerIterCount,nodeName=${settings.network.nodeName},block=${Algos.encode(blockHeader.id)}," +
                s"height=${blockHeader.height} value=${blockHeader.nonce - Long.MaxValue / workersQty * workerIdx + 1}"
            )
          )
        }

    case StartApplyingModif(modifierId: ModifierId, modifierTypeId: ModifierTypeId, startTime: Long) =>
      modifiersToApply += Algos.encode(modifierId) -> (modifierTypeId, startTime)

    case EndOfApplyingModif(modifierId) =>
      modifiersToApply.get(Algos.encode(modifierId)).foreach { modInfo =>
        influxDB.write(settings.influxDB.udpPort, s"modifApplying,nodeName=${settings.network.nodeName}," +
          s"modType=${modInfo._1} value=${System.currentTimeMillis() - modInfo._2}")
        modifiersToApply -= Algos.encode(modifierId)
      }

    case TransactionGeneratorStat(txsQty: Int, generationTime: Long) =>
      influxDB.write(settings.influxDB.udpPort, s"transactionGenerator,nodeName=${settings.network.nodeName} txsQty=$txsQty,generationTime=$generationTime")

    case SleepTime(time: Long) =>
      influxDB.write(settings.influxDB.udpPort, s"sleepTime,nodeName=${settings.network.nodeName} value=$time")

    case MiningTime(time: Long) =>
      influxDB.write(settings.influxDB.udpPort, s"miningTime,nodeName=${settings.network.nodeName} value=$time")

    case CandidateProducingTime(time: Long) =>
      influxDB.write(settings.influxDB.udpPort, s"candidateProducing,nodeName=${settings.network.nodeName} value=$time")

    case StateUpdating(time: Long) =>
      influxDB.write(settings.influxDB.udpPort, s"stateUpdatingTime,nodeName=${settings.network.nodeName} value=$time")

    case SendDownloadRequest(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId]) =>
      modifiersToDownload = modifiersToDownload ++ modifiers.map(mod => (Algos.encode(mod), (modifierTypeId, System.currentTimeMillis())))

    case GetModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId]) =>
      modifiers.foreach(downloadedModifierId =>
        modifiersToDownload.get(Algos.encode(downloadedModifierId)).foreach { dowloadInfo =>
          influxDB.write(
            settings.influxDB.udpPort,
            s"modDownloadStat,nodeName=${settings.network.nodeName},modId=${Algos.encode(downloadedModifierId)}," +
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

  case class MiningEnd(blockHeader: EncryBlockHeader, workerIdx: Int, workersQty: Int)

  case class MiningTime(time: Long)

  case class BestHeaderInChain(bestHeader: EncryBlockHeader)

  case class SendDownloadRequest(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId])

  case class GetModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId])

  case class BlocksStat(notCompletedBlocks: Int, headerCache: Int, payloadCache: Int, completedBlocks: Int)

  //Todo no use

  case class StateUpdating(time: Long)

  case class TransactionGeneratorStat(txsQty: Int, generationTime: Long)
}
