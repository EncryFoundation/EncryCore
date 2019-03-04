package encry.view.state

import java.io.File

import com.typesafe.scalalogging.StrictLogging
import encry.utils.CoreTaggedTypes.VersionTag
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.{ADProofs, Block, Header}
import encry.modifiers.mempool.Transaction
import encry.settings.{Constants, EncryAppSettings, LevelDBSettings, NodeSettings}
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{LevelDBVersion, VersionalLevelDbKey, VersionalLevelDbValue}
import encry.storage.levelDb.versionalLevelDB._
import encry.view.state.UtxoState.logger
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.encryfoundation.common.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADDigest
import org.iq80.leveldb.Options

import scala.util.{Failure, Success, Try}

class DigestState protected(override val version: VersionTag,
                            override val rootHash: ADDigest,
                            val stateStore: VersionalStorage,
                            settings: NodeSettings)
  extends EncryState[DigestState] with ModifierValidation[EncryPersistentModifier] with StrictLogging {

  assert(version sameElements stateStore.currentVersion, "`version` should always be equal to store.lastVersionID")

  val maxRollbackDepth: Int = stateStore.versions.size

  def validate(mod: EncryPersistentModifier): Try[Unit] = mod match {
    case block: Block =>
      Try {
        if (!ADProofs.proofDigest(block.adProofsOpt.get.proofBytes).sameElements(block.header.adProofsRoot))
          Failure(new Exception(s"Got invalid Proof for block: $block"))

        val txs: Seq[Transaction] = block.payload.transactions
        val declaredHash: ADDigest = block.header.stateRoot

        //TODO: refactor
        txs.foldLeft(Success(): Try[Unit]) { case (status, tx) =>
          status.flatMap(_ => if (tx.semanticValidity.isSuccess) Success(tx)
                              else util.Failure(tx.semanticValidity.errors.head.toThrowable)
          )}.flatMap(_ => block.adProofsOpt.map(_.verify(extractStateChanges(txs), rootHash, declaredHash))
          .getOrElse(Failure(new Exception("Proofs are empty"))))
      }.flatten match {
        case s: Success[_] =>
          logger.info(s"Valid modifier applied to DigestState: ${block.encodedId}")
          s
        case Failure(e) =>
          logger.warn(s"Modifier $mod is not valid: $e")
          Failure(e)
      }
    case mod: Any =>
      Failure(new Exception(s"Unhandled modifier: $mod"))
  }

  private def update(newVersion: VersionTag, newRootHash: ADDigest): Try[DigestState] = Try {
    val wrappedVersion: ByteArrayWrapper = ByteArrayWrapper(newVersion)
    stateStore.insert(
      StorageVersion @@ wrappedVersion.data,
      List(StorageKey @@ wrappedVersion.data -> StorageValue @@ newRootHash.untag(ADDigest))
    )
    new DigestState(newVersion, newRootHash, stateStore, settings)
  }

  override def applyModifier(mod: EncryPersistentModifier): Try[DigestState] = mod match {
    case block: Block if settings.verifyTransactions =>
      logger.info(s"Got new full block with id ${block.encodedId} " +
        s"with root ${Algos.encoder.encode(block.header.stateRoot)}")
      this.validate(block).flatMap(_ => update(VersionTag !@@ block.header.id, block.header.stateRoot))

    case header: Header if !settings.verifyTransactions =>
      logger.info(s"Got new Header ${header.encodedId} with root ${Algos.encoder.encode(header.stateRoot)}")
      update(VersionTag !@@ header.id, header.stateRoot)

    case a: Any =>
      logger.info(s"Unhandled modifier: $a")
      Failure(new Exception(s"Unhandled modifier: $mod"))
  }

  override def rollbackTo(version: VersionTag): Try[DigestState] = {
    logger.info(s"Rollback Digest State to version ${Algos.encoder.encode(version)}")
    val wrappedVersion: ByteArrayWrapper = ByteArrayWrapper(version)
    Try(stateStore.rollbackTo(StorageVersion @@ wrappedVersion.data)).map { _ =>
      val rootHash: ADDigest = ADDigest @@ stateStore.get(StorageKey @@ wrappedVersion.data).get.untag(VersionalLevelDbValue)
      logger.info(s"Rollback to version ${Algos.encoder.encode(version)} with roothash " +
        s"${Algos.encoder.encode(rootHash)}")
      new DigestState(version, rootHash, stateStore, settings)
    }
  }

  override def rollbackVersions: Iterable[VersionTag] = stateStore.versions.map(VersionTag @@ _.untag(LevelDBVersion))
}

object DigestState extends StrictLogging {

  def create(versionOpt: Option[VersionTag],
             rootHashOpt: Option[ADDigest],
             dir: File,
             settings: EncryAppSettings): DigestState = Try {
    val vldbInit = settings.storage.state match {
      case VersionalStorage.IODB =>
        logger.info("Init state with iodb storage")
        IODBWrapper(new LSMStore(dir, keepVersions = Constants.DefaultKeepVersions))
      case VersionalStorage.LevelDB =>
        logger.info("Init state with levelDB storage")
        val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
        VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, LevelDBSettings(300, 33), keySize = 33))
    }

    (versionOpt, rootHashOpt) match {

      case (Some(version), Some(rootHash)) =>
        if (vldbInit.currentVersion sameElements version) {
          new DigestState(version, rootHash, vldbInit, settings.node)
        } else {
          val inVersion: VersionTag = VersionTag @@ vldbInit.currentVersion.untag(LevelDBVersion)
          new DigestState(inVersion, rootHash, vldbInit, settings.node).update(version, rootHash).get //sync store
        }.ensuring(vldbInit.currentVersion.sameElements(version))

      case (None, None) =>
        val version: VersionTag = VersionTag @@ vldbInit.currentVersion.untag(LevelDBVersion)
        val rootHash: Array[Byte] = vldbInit.get(StorageKey @@ version.untag(VersionTag)).get

        new DigestState(version, ADDigest @@ rootHash, vldbInit, settings.node)

      case _ => throw new Exception("Unsupported argument combination.")
    }
  }.getOrElse(EncryState.generateGenesisDigestState(dir, settings))
}