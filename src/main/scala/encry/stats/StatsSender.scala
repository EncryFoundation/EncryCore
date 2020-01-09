package encry.stats

import java.io.File
import java.net.InetAddress
import java.util
import java.text.SimpleDateFormat
import akka.actor.{Actor, Props}
import com.typesafe.scalalogging.StrictLogging
import encry.EncryApp.timeProvider
import encry.consensus.EncrySupplyController
import encry.settings.{InfluxDBSettings, NetworkSettings}
import encry.stats.StatsSender._
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Height, ModifierId, ModifierTypeId}
import org.encryfoundation.common.utils.constants.Constants
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scala.concurrent.ExecutionContext.Implicits.global

class StatsSender(influxDBSettings: InfluxDBSettings, networkSettings: NetworkSettings, constants: Constants) extends Actor with StrictLogging {

  var modifiersToDownload: Map[String, (ModifierTypeId, Long)] = Map.empty
  var modifiersToApply: Map[String, (ModifierTypeId, Long)] = Map.empty

  val nodeName: String = networkSettings.nodeName match {
    case Some(value) => value
    case None => InetAddress.getLocalHost.getHostAddress + ":" + networkSettings.bindAddress.getPort
  }
  val influxDB: InfluxDB = InfluxDBFactory
    .connect(influxDBSettings.url, influxDBSettings.login, influxDBSettings.password)
  influxDB.setRetentionPolicy("autogen")

  override def preStart(): Unit = influxDB.write(influxDBSettings.udpPort, s"""nodesStartTime value="$nodeName"""")

  val sdf: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  override def receive: Receive = {
    case BestHeaderInChain(fb) =>
      influxDB.write(
        influxDBSettings.udpPort,
        util.Arrays.asList(
          s"difficulty,nodeName=$nodeName diff=${fb.difficulty.toString},height=${fb.height}", //++
          s"""height,nodeName=$nodeName header="${fb.encodedId}",height=${fb.height}""", //++
          s"stateWeight,nodeName=$nodeName,height=${fb.height} " +
            s"value=${new File("encry/data/state/").listFiles.foldLeft(0L)(_ + _.length())}", //++
          s"historyWeight,nodeName=$nodeName,height=${fb.height} " +
            s"value=${new File("encry/data/history/").listFiles.foldLeft(0L)(_ + _.length())}", //++
          s"supply,nodeName=$nodeName,height=${fb.height} " +
            s"value=${EncrySupplyController.supplyAt(fb.height.asInstanceOf[Height], constants)}" //++
        ))

    case HeightStatistics(bestHeaderHeight, bestBlockHeight) =>
      println(s"HeightStatistics")
      influxDB.write(
        influxDBSettings.udpPort,
        s"chainStat,nodeName=$nodeName value=$bestHeaderHeight,bestBlockHeight=$bestBlockHeight"
      )

    case TransactionsInBlock(txsNum) =>
      influxDB.write(influxDBSettings.udpPort, s"txsInEachBlock,nodeName=$nodeName value=$txsNum")

    case ModifierAppendedToHistory(isHeader, success) if nodeName.exists(_.isDigit) =>
      val nodeNumber: Long = nodeName.filter(_.isDigit).toLong
      influxDB.write(
        influxDBSettings.udpPort,
        s"""modifierAppendedToHistory,success=$success,isHeader=$isHeader,nodeName=$nodeName value=$nodeNumber"""
      )

    case ModifierAppendedToState(success) if nodeName.exists(_.isDigit) =>
      val nodeNumber: Long = nodeName.filter(_.isDigit).toLong
      influxDB.write(
        influxDBSettings.udpPort,
        s"""modifierAppendedToState,success=$success,nodeName=$nodeName value=$nodeNumber"""
      )

    case InfoAboutTransactionsFromMiner(qty) =>
      influxDB.write(influxDBSettings.udpPort, s"infoAboutTxsFromMiner,nodeName=$nodeName value=$qty")

    case GetModifiers(_, modifiers) => modifiers
      .foreach(downloadedModifierId =>
        modifiersToDownload.get(Algos.encode(downloadedModifierId)).foreach { dowloadInfo =>
          influxDB.write(
            influxDBSettings.udpPort,
            s"modDownloadStat,nodeName=$nodeName,modId=${Algos.encode(downloadedModifierId)}," +
              s"modType=${dowloadInfo._1} value=${System.currentTimeMillis() - dowloadInfo._2}"
          )
          modifiersToDownload = modifiersToDownload - Algos.encode(downloadedModifierId)
        }
      )

    case MiningEnd(blockHeader, workerIdx, workersQty) => timeProvider
      .time()
      .map(time => influxDB.write(
        influxDBSettings.udpPort,
        util.Arrays.asList(
          s"miningEnd,nodeName=$nodeName,block=${Algos.encode(blockHeader.id)}," +
            s"height=${blockHeader.height},worker=$workerIdx value=${time - blockHeader.timestamp}",
          s"minerIterCount,nodeName=$nodeName,block=${Algos.encode(blockHeader.id)}," +
            s"height=${blockHeader.height} value=${blockHeader.nonce - Long.MaxValue / workersQty * workerIdx + 1}"
        )))

    case EndOfApplyingModifier(modifierId) =>
      modifiersToApply.get(Algos.encode(modifierId)).foreach { modInfo =>
        influxDB.write(influxDBSettings.udpPort, s"modifApplying,nodeName=$nodeName," +
          s"modType=${modInfo._1} value=${System.currentTimeMillis() - modInfo._2}")
        modifiersToApply -= Algos.encode(modifierId)
      }

    case StartApplyingModifier(modifierId, modifierTypeId, startTime) =>
      modifiersToApply += Algos.encode(modifierId) -> (modifierTypeId, startTime)

    case MiningTime(time) => influxDB.write(influxDBSettings.udpPort, s"miningTime,nodeName=$nodeName value=$time")

    case SleepTime(time) => influxDB.write(influxDBSettings.udpPort, s"sleepTime,nodeName=$nodeName value=$time")

    case StateUpdating(time) => influxDB.write(influxDBSettings.udpPort, s"stateUpdatingTime,nodeName=$nodeName value=$time")

    case msg: ModifiersDownloadStatistic => msg match {
      case _ if nodeName.exists(_.isDigit) =>
        val nodeNumber: Long = nodeName.filter(_.isDigit).toLong
        val (isHeader: Boolean, tableName: String) = msg match {
          case SerializedModifierFromNetwork(t) =>
            (t == Header.modifierTypeId) -> "serializedModifierFromNetwork"
          case ValidatedModifierFromNetwork(t) =>
            (t == Header.modifierTypeId) -> "validatedModifierFromNetwork"
        }
        influxDB.write(
          influxDBSettings.udpPort,
          s"""$tableName,nodeName=$nodeNumber,isHeader=$isHeader value=$nodeNumber"""
        )
      case _ => //do nothing
    }

    case ModifierAppendedToHistory(_, _) =>
    case ModifierAppendedToState(_) =>

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
  final case class StartApplyingModifier(modifierId: ModifierId, modifierTypeId: ModifierTypeId, startTime: Long)
  final case class MiningEnd(blockHeader: Header, workerIdx: Int, workersQty: Int)
  final case class MiningTime(time: Long) extends AnyVal
  final case class SendDownloadRequest(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId])
  final case class GetModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId])
  sealed trait ModifiersDownloadStatistic
  final case class SerializedModifierFromNetwork(modifierTypeId: ModifierTypeId) extends ModifiersDownloadStatistic
  final case class ValidatedModifierFromNetwork(modifierTypeId: ModifierTypeId) extends ModifiersDownloadStatistic

  final case class NewHeightByHistory(height: Int) extends AnyVal
  final case class NewHeightByState(height: Int) extends AnyVal

  def props(influxDBSettings: InfluxDBSettings, networkSettings: NetworkSettings, constants: Constants): Props =
    Props(new StatsSender(influxDBSettings, networkSettings, constants))
}