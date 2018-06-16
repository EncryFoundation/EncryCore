package encry.stats

import java.io.File
import java.util

import akka.actor.Actor
import encry.EncryApp.{settings, timeProvider}
import encry.consensus.DifficultySerializer
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.settings.Algos
import encry.stats.StatsSender.{DownloadModifierEnd, DownloadModifierStart, MiningEnd}
import encry.utils.ScorexLogging
import org.influxdb.{InfluxDB, InfluxDBFactory}
import scorex.core.{ModifierId, ModifierTypeId}

import scala.collection.JavaConverters._

class StatsSender extends Actor with ScorexLogging {

  val influxDB: InfluxDB =
    InfluxDBFactory.connect(settings.influxDB.url, settings.influxDB.login, settings.influxDB.password )

  influxDB.setRetentionPolicy("autogen")

  var minedBlocks: Map[ModifierId, Long] = Map.empty[ModifierId, Long]

  var toDownloadModifier: Map[ModifierId, (Long, ModifierTypeId)] = Map.empty[ModifierId, (Long, ModifierTypeId)]

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
    influxDB.write(8189, util.Arrays.asList(
      s"nodesStartTime value=${settings.network.nodeName}"
    ))
  }

  override def receive: Receive = {

    case SemanticallySuccessfulModifier(fb: EncryBlock) =>

      influxDB.write(8189, util.Arrays.asList(
        minedBlocks.get(fb.header.id).map( value =>
          s"miningEnd,nodeName=${settings.network.nodeName} value=$value"
        ).getOrElse(""),
        s"difficulty,nodeName=${settings.network.nodeName} value=${DifficultySerializer.decodeCompactBits(fb.header.nBits)}",
        s"height,nodeName=${settings.network.nodeName} value=${fb.header.height}",
        s"txsInBlock,nodeName=${settings.network.nodeName} value=${fb.payload.transactions.length}",
        s"lastBlockSize,nodeName=${settings.network.nodeName} value=${fb.bytes.length}",
        s"stateSize,nodeName=${settings.network.nodeName} value=${new File("encry/data/state/").listFiles.foldLeft(0L)(_ + _.length)/1024}",
        s"historySize,nodeName=${settings.network.nodeName} value=${new File("encry/data/history/").listFiles.foldLeft(0L)(_ + _.length)/1024}"
        )
      )

      minedBlocks = Map.empty[ModifierId, Long]

    case MiningEnd(blockHeader: EncryBlockHeader, workerNumber: Int) =>

      minedBlocks += (blockHeader.id -> (timeProvider.time() - blockHeader.timestamp)/1000L)

    case DownloadModifierStart(modifierTypeId: ModifierTypeId, modifiersId: Seq[ModifierId]) =>

      toDownloadModifier = toDownloadModifier ++ modifiersId.map(_ -> (System.currentTimeMillis(), modifierTypeId)).toMap

    case DownloadModifierEnd(modifiersId: Seq[ModifierId]) =>

      influxDB.write(8189, modifiersId.flatMap(id => {
        toDownloadModifier.get(id).map(modifierDownloadInfo => {
          toDownloadModifier -= id
          (System.currentTimeMillis() - modifierDownloadInfo._1, modifierDownloadInfo._2, id)
        })
      }).map(modDownloadInfo =>
        s"modifierDownloadInfo,nodeName=${settings.network.nodeName},modId=${Algos.encode(modDownloadInfo._3)},modTypeId=${modDownloadInfo._2} value=${modDownloadInfo._1/1000L}"
      ).asJava)

  }
}

object StatsSender {

  case class DownloadModifierStart(modifierTypeId: ModifierTypeId, modifiersId: Seq[ModifierId])

  case class DownloadModifierEnd(modifiersId: Seq[ModifierId])

  case class MiningEnd(blockHeader: EncryBlockHeader, workerNumber: Int)
}
