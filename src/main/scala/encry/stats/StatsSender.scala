package encry.stats

import java.io.File
import java.util

import akka.actor.Actor
import encry.EncryApp.{settings, timeProvider}
import encry.consensus.emission.EncrySupplyController
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.Algos
import encry.stats.StatsSender._
import encry.utils.ScorexLogging
import encry.view.history
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scorex.core.{ModifierId, ModifierTypeId}

class StatsSender extends Actor with ScorexLogging {

  val influxDB: InfluxDB =
    InfluxDBFactory.connect(settings.influxDB.url, settings.influxDB.login, settings.influxDB.password )

  var modifiersToDownload: Map[ModifierId, (ModifierTypeId, Long)] = Map.empty[ModifierId, (ModifierTypeId, Long)]

  influxDB.setRetentionPolicy("autogen")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[BestHeaderInChain])
    context.system.eventStream.subscribe(self, classOf[SendDownloadRequest])
    context.system.eventStream.subscribe(self, classOf[GetModifiers])
    influxDB.write(8189, s"nodesStartTime value=" + '\"' + settings.network.nodeName + '\"')
  }

  override def receive: Receive = {

    case BestHeaderInChain(fb: EncryBlockHeader) =>

        influxDB.write(8189, util.Arrays.asList(
          s"difficulty,nodeName=${settings.network.nodeName} diff=${fb.difficulty.toString},height=${fb.height}",
          s"height,nodeName=${settings.network.nodeName},header=${Algos.encode(fb.id)} height=${fb.height}",
          s"stateWeight,nodeName=${settings.network.nodeName},height=${fb.height} value=${new File("encry/data/state/").listFiles.foldLeft(0L)(_ + _.length())}",
          s"historyWeight,nodeName=${settings.network.nodeName},height=${fb.height} value=${new File("encry/data/history/").listFiles.foldLeft(0L)(_ + _.length())}",
          s"supply,nodeName=${settings.network.nodeName},height=${fb.height} value=${EncrySupplyController.supplyAt(fb.height.asInstanceOf[history.Height])}"
        )
        )

    case MiningEnd(blockHeader: EncryBlockHeader, workerNumber: Int) =>

      influxDB.write(8189,
        s"miningEnd,nodeName=${settings.network.nodeName},block=${Algos.encode(blockHeader.id)},height=${blockHeader.height},worker=$workerNumber value=${(timeProvider.time() - blockHeader.timestamp)/1000L}"
      )

//    case SendErrorMsgToStat(error: String) =>
//      influxDB.write(8189, s"error node=${settings.network.nodeName} value=" + '\"' + error + '\"')

    case SendDownloadRequest(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId]) =>
      modifiersToDownload = modifiersToDownload ++ modifiers.map(_ -> (modifierTypeId, System.currentTimeMillis()))

    case GetModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId]) =>
      modifiers.foreach(downloadedModifierId =>
        modifiersToDownload.get(downloadedModifierId).map(dowloadInfo =>
          influxDB.write(8189,
            s"modDownloadStat,nodeName=${settings.network.nodeName},modId=${Algos.encode(downloadedModifierId)},modType=${dowloadInfo._1} value=${(System.currentTimeMillis() - dowloadInfo._2)/1000L}"
          )
        )
      )
  }
}

object StatsSender {

  case class MiningEnd(blockHeader: EncryBlockHeader, workerNumber: Int)

  case class BestHeaderInChain(bestHeader: EncryBlockHeader)

  case class SendErrorMsgToStat(error: String)

  case class SendDownloadRequest(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId])

  case class GetModifiers(modifierTypeId: ModifierTypeId, modifiers: Seq[ModifierId])
}
