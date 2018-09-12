package encry.stats

import java.io.File
import java.net.InetAddress
import java.util
import java.text.SimpleDateFormat
import akka.actor.Actor
import encry.utils.CoreTaggedTypes.{ModifierId, ModifierTypeId}
import encry.EncryApp.{settings, timeProvider}
import encry.consensus.EncrySupplyController
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.stats.StatsSender._
import encry.view.history
import encry.stats.LoggingActor.LogMessage
import encry.view.history.History.Height
import org.encryfoundation.common.Algos
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

class StatsSender extends Actor {

  var modifiersToDownload: Map[String, (ModifierTypeId, Long)] = Map()

  val nodeName: String = settings.network.nodeName match {
    case Some(value) => value
    case None => InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort
  }

  val influxDB: InfluxDB =
    InfluxDBFactory.connect(settings.influxDB.url, settings.influxDB.login, settings.influxDB.password)

  influxDB.setRetentionPolicy("autogen")

  val modifiersToApply: mutable.Map[String, (ModifierTypeId, Long)] = mutable.Map[String, (ModifierTypeId, Long)]()

  override def preStart(): Unit =
    influxDB.write(settings.influxDB.udpPort, s"""nodesStartTime value="$nodeName"""")

  val sdf: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  override def receive: Receive = {
    case TxsInBlock(txsNum) =>
      influxDB.write(settings.influxDB.udpPort, s"txsInEachBlock,nodeName=$nodeName value=$txsNum")
    case TxsInBlockchain(qty) =>
      influxDB.write(settings.influxDB.udpPort, s"txsInBlocks,nodeName=$nodeName value=$qty")
    case DiffBtwMempoolAndLastBlockTxs(num) =>
      influxDB.write(settings.influxDB.udpPort, s"txsDiff,nodeName=$nodeName value=$num")
    case MempoolStat(size) =>
      influxDB.write(settings.influxDB.udpPort, s"txsInMempool,nodeName=$nodeName value=$size")
    case TransactionsStatMessage(num, height) =>
      influxDB.write(settings.influxDB.udpPort, s"numOfTxsInBlock,nodeName=$nodeName value=$num,height=$height")
    case LogMessage(logLevel, logMessage, logTime) => influxDB.write(settings.influxDB.udpPort,
      s"""logsFromNode,nodeName=$nodeName,logLevel=${
        logLevel match {
          case "Info" => 1
          case "Debug" => 2
          case "Warn" => 3
          case "Error" => 4
          case _ => 4
        }
      } value="[${sdf.format(logTime)}] $logMessage"""")
    case HeightStatistics(bestHeaderHeight, bestBlockHeight) =>
      influxDB.write(settings.influxDB.udpPort,
        s"chainStat,nodeName=$nodeName value=$bestHeaderHeight,bestBlockHeight=$bestBlockHeight")
    case BestHeaderInChain(fb: EncryBlockHeader) =>
      influxDB.write(settings.influxDB.udpPort, util.Arrays.asList(
        s"difficulty,nodeName=$nodeName diff=${fb.difficulty.toString},height=${fb.height}",
        s"height,nodeName=$nodeName,header=${Algos.encode(fb.id)} height=${fb.height}",
        s"stateWeight,nodeName=$nodeName,height=${fb.height} " +
          s"value=${new File("encry/data/state/").listFiles.foldLeft(0L)(_ + _.length())}",
        s"historyWeight,nodeName=$nodeName,height=${fb.height} " +
          s"value=${new File("encry/data/history/").listFiles.foldLeft(0L)(_ + _.length())}",
        s"supply,nodeName=$nodeName,height=${fb.height} " +
          s"value=${EncrySupplyController.supplyAt(fb.height.asInstanceOf[Height])}"
      )
      )

    case MiningEnd(blockHeader: EncryBlockHeader, workerIdx: Int, workersQty: Int) =>
      timeProvider
        .time()
        .map { time =>
          influxDB.write(
            settings.influxDB.udpPort,
            util.Arrays.asList(
              s"miningEnd,nodeName=$nodeName,block=${Algos.encode(blockHeader.id)}," +
                s"height=${blockHeader.height},worker=$workerIdx value=${time - blockHeader.timestamp}",
              s"minerIterCount,nodeName=$nodeName,block=${Algos.encode(blockHeader.id)}," +
                s"height=${blockHeader.height} value=${blockHeader.nonce - Long.MaxValue / workersQty * workerIdx + 1}"
            )
          )
        }

    case StartApplyingModif(modifierId: ModifierId, modifierTypeId: ModifierTypeId, startTime: Long) =>
      modifiersToApply += Algos.encode(modifierId) -> (modifierTypeId, startTime)

    case EndOfApplyingModif(modifierId) =>
      modifiersToApply.get(Algos.encode(modifierId)).foreach { modInfo =>
        influxDB.write(settings.influxDB.udpPort, s"modifApplying,nodeName=$nodeName," +
          s"modType=${modInfo._1} value=${System.currentTimeMillis() - modInfo._2}")
        modifiersToApply -= Algos.encode(modifierId)
      }

    case TransactionGeneratorStat(txsQty: Int, generationTime: Long) =>
      influxDB.write(settings.influxDB.udpPort, s"transactionGenerator,nodeName=$nodeName txsQty=$txsQty,generationTime=$generationTime")

    case SleepTime(time: Long) =>
      influxDB.write(settings.influxDB.udpPort, s"sleepTime,nodeName=$nodeName value=$time")

    case MiningTime(time: Long) =>
      influxDB.write(settings.influxDB.udpPort, s"miningTime,nodeName=$nodeName value=$time")

    case CandidateProducingTime(time: Long) =>
      influxDB.write(settings.influxDB.udpPort, s"candidateProducing,nodeName=$nodeName value=$time")

    case StateUpdating(time: Long) =>
      influxDB.write(settings.influxDB.udpPort, s"stateUpdatingTime,nodeName=$nodeName value=$time")

    case SendDownloadRequest(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId]) =>
      modifiersToDownload = modifiersToDownload ++ modifiers.map(mod => (Algos.encode(mod), (modifierTypeId, System.currentTimeMillis())))

    case GetModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId]) =>
      modifiers.foreach(downloadedModifierId =>
        modifiersToDownload.get(Algos.encode(downloadedModifierId)).foreach { dowloadInfo =>
          influxDB.write(
            settings.influxDB.udpPort,
            s"modDownloadStat,nodeName=$nodeName,modId=${Algos.encode(downloadedModifierId)}," +
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

  case class HeightStatistics(bestHeaderHeight: Int, bestBlockHeight: Int)

  case class StateUpdating(time: Long)

  case class TransactionGeneratorStat(txsQty: Int, generationTime: Long)

  case class TransactionsStatMessage(transactionsNum: Int, blockHeight: Int)

  case class MempoolStat(size: Int)

  case class DiffBtwMempoolAndLastBlockTxs(diff: Int)

  case class TxsInBlockchain(qty: Int)

  case class TxsInBlock(txsNum: Int)

}
