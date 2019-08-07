package encry.stats

import java.io.File
import java.net.InetAddress
import java.util
import java.text.SimpleDateFormat
import akka.actor.Actor
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.{settings, timeProvider}
import encry.consensus.EncrySupplyController
import encry.stats.StatsSender._
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Height, ModifierId, ModifierTypeId}
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

class StatsSender extends Actor with StrictLogging {

  var modifiersToDownload: Map[String, (ModifierTypeId, Long)] = Map.empty
  val modifiersToApply: mutable.Map[String, (ModifierTypeId, Long)] = mutable.Map.empty

  val nodeName: String = settings.network.nodeName match {
    case Some(value) => value
    case None        => InetAddress.getLocalHost.getHostAddress + ":" + settings.network.bindAddress.getPort
  }
  val InfluxURL: String = settings
    .influxDB.map(_.url)
    .getOrElse(throw new RuntimeException("url not specified"))
  val InfluxLogin: String = settings
    .influxDB.map(_.login)
    .getOrElse(throw new RuntimeException("login not specified"))
  val InfluxPassword: String = settings
    .influxDB.map(_.password)
    .getOrElse(throw new RuntimeException("password not specified"))
  val InfluxPort: Int = settings
    .influxDB.map(_.udpPort)
    .getOrElse(throw new RuntimeException("udp port not specified"))
  val influxDB: InfluxDB = InfluxDBFactory.connect(InfluxURL, InfluxLogin, InfluxPassword)
  influxDB.setRetentionPolicy("autogen")

  override def preStart(): Unit = influxDB.write(InfluxPort, s"""nodesStartTime value="$nodeName"""")

  val sdf: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  override def receive: Receive = {
    case BestHeaderInChain(fb) =>
      influxDB.write(
        InfluxPort,
        util.Arrays.asList(
          s"difficulty,nodeName=$nodeName diff=${fb.difficulty.toString},height=${fb.height}", //++
          s"""height,nodeName=$nodeName header="${fb.encodedId}",height=${fb.height}""", //++
          s"stateWeight,nodeName=$nodeName,height=${fb.height} " +
            s"value=${new File("encry/data/state/").listFiles.foldLeft(0L)(_ + _.length())}", //++
          s"historyWeight,nodeName=$nodeName,height=${fb.height} " +
            s"value=${new File("encry/data/history/").listFiles.foldLeft(0L)(_ + _.length())}", //++
          s"supply,nodeName=$nodeName,height=${fb.height} " +
            s"value=${EncrySupplyController.supplyAt(fb.height.asInstanceOf[Height])}" //++
        ))

    case HeightStatistics(bestHeaderHeight, bestBlockHeight) =>
      influxDB.write(
        InfluxPort,
        s"chainStat,nodeName=$nodeName value=$bestHeaderHeight,bestBlockHeight=$bestBlockHeight"
      )

    case TransactionsInBlock(txsNum) => influxDB.write(InfluxPort, s"txsInEachBlock,nodeName=$nodeName value=$txsNum")

    case ModifierAppendedToHistory(isHeader, success) if nodeName.exists(_.isDigit) =>
      val nodeNumber: Long = nodeName.filter(_.isDigit).toLong
      influxDB.write(
        InfluxPort,
        s"""modifierAppendedToHistory,success=$success,isHeader=$isHeader,nodeName=$nodeName value=$nodeNumber"""
      )

    case ModifierAppendedToState(success) if nodeName.exists(_.isDigit) =>
      val nodeNumber: Long = nodeName.filter(_.isDigit).toLong
      influxDB.write(
        InfluxPort,
        s"""modifierAppendedToState,success=$success,nodeName=$nodeName value=$nodeNumber"""
      )

    case InfoAboutTransactionsFromMiner(qty) =>
      influxDB.write(InfluxPort, s"infoAboutTxsFromMiner,nodeName=$nodeName value=$qty")

    case GetModifiers(_, modifiers) => modifiers
      .foreach(downloadedModifierId =>
        modifiersToDownload.get(Algos.encode(downloadedModifierId)).foreach { dowloadInfo =>
          influxDB.write(
            InfluxPort,
            s"modDownloadStat,nodeName=$nodeName,modId=${Algos.encode(downloadedModifierId)}," +
              s"modType=${dowloadInfo._1} value=${System.currentTimeMillis() - dowloadInfo._2}"
          )
          modifiersToDownload = modifiersToDownload - Algos.encode(downloadedModifierId)
        }
      )

    case MiningEnd(blockHeader, workerIdx, workersQty) => timeProvider
      .time()
      .map(time => influxDB.write(
        InfluxPort,
        util.Arrays.asList(
          s"miningEnd,nodeName=$nodeName,block=${Algos.encode(blockHeader.id)}," +
            s"height=${blockHeader.height},worker=$workerIdx value=${time - blockHeader.timestamp}",
          s"minerIterCount,nodeName=$nodeName,block=${Algos.encode(blockHeader.id)}," +
            s"height=${blockHeader.height} value=${blockHeader.nonce - Long.MaxValue / workersQty * workerIdx + 1}"
        )))

    case EndOfApplyingModifier(modifierId) =>
      modifiersToApply.get(Algos.encode(modifierId)).foreach { modInfo =>
        influxDB.write(InfluxPort, s"modifApplying,nodeName=$nodeName," +
          s"modType=${modInfo._1} value=${System.currentTimeMillis() - modInfo._2}")
        modifiersToApply -= Algos.encode(modifierId)
      }

    case StartApplyingModif(modifierId, modifierTypeId, startTime) =>
      modifiersToApply += Algos.encode(modifierId) -> (modifierTypeId, startTime)

    case MiningTime(time) => influxDB.write(InfluxPort, s"miningTime,nodeName=$nodeName value=$time")

    case SleepTime(time) => influxDB.write(InfluxPort, s"sleepTime,nodeName=$nodeName value=$time")

    case StateUpdating(time) => influxDB.write(InfluxPort, s"stateUpdatingTime,nodeName=$nodeName value=$time")

    case SendDownloadRequest(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId]) =>
      modifiersToDownload = modifiersToDownload ++ modifiers.map(mod => (Algos.encode(mod), (modifierTypeId, System.currentTimeMillis())))
  }
}

object StatsSender {
  final case class BestHeaderInChain(bestHeader: Header) extends AnyVal
  final case class HeightStatistics(bestHeaderHeight: Int, bestBlockHeight: Int)
  final case class TransactionsInBlock(txsNum: Int) extends AnyVal
  final case class ModifierAppendedToHistory(isHeader: Boolean, success: Boolean)
  final case class ModifierAppendedToState(success: Boolean) extends AnyVal
  final case class InfoAboutTransactionsFromMiner(qty: Int) extends AnyVal
  final case class EndOfApplyingModifier(modifierId: ModifierId) extends AnyVal
  final case class StateUpdating(time: Long) extends AnyVal
  final case class SleepTime(time: Long) extends AnyVal
  final case class StartApplyingModif(modifierId: ModifierId, modifierTypeId: ModifierTypeId, startTime: Long)
  final case class MiningEnd(blockHeader: Header, workerIdx: Int, workersQty: Int)
  final case class MiningTime(time: Long) extends AnyVal
  final case class SendDownloadRequest(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId])
  final case class GetModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId])
}