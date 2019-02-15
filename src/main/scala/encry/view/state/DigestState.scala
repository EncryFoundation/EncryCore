package encry.view.state

import java.io.File

import com.typesafe.scalalogging.StrictLogging
import encry.utils.CoreTaggedTypes.VersionTag
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.{ADProofs, Block, Header}
import encry.modifiers.mempool.Transaction
import encry.settings.{Constants, NodeSettings}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import org.encryfoundation.common.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADDigest

import scala.util.{Failure, Success, Try}

class DigestState protected(override val version: VersionTag,
                            override val rootHash: ADDigest,
                            val stateStore: Store,
                            settings: NodeSettings)
  extends EncryState[DigestState] with ModifierValidation[EncryPersistentModifier] with StrictLogging {

  stateStore.lastVersionID
    .foreach(id => assert(version sameElements id.data, "`version` should always be equal to store.lastVersionID"))

  val maxRollbackDepth: Int = stateStore.rollbackVersions().size

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
          //logger.info(s"Valid modifier applied to DigestState: ${block.encodedId}")
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
    stateStore.update(wrappedVersion, toRemove = Seq(), toUpdate = Seq(wrappedVersion -> ByteArrayWrapper(newRootHash)))
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
    Try(stateStore.rollback(wrappedVersion)).map { _ =>
      stateStore.clean(Constants.DefaultKeepVersions)
      val rootHash: ADDigest = ADDigest @@ stateStore.get(wrappedVersion).get.data
      logger.info(s"Rollback to version ${Algos.encoder.encode(version)} with roothash " +
        s"${Algos.encoder.encode(rootHash)}")
      new DigestState(version, rootHash, stateStore, settings)
    }
  }

  override def rollbackVersions: Iterable[VersionTag] = stateStore.rollbackVersions().map(VersionTag @@ _.data)
}

object DigestState {

  def create(versionOpt: Option[VersionTag],
             rootHashOpt: Option[ADDigest],
             dir: File,
             settings: NodeSettings): DigestState = Try {
    val store = new LSMStore(dir, keepVersions = Constants.DefaultKeepVersions)

    (versionOpt, rootHashOpt) match {

      case (Some(version), Some(rootHash)) =>
        if (store.lastVersionID.isDefined && store.lastVersionID.forall(_.data sameElements version)) {
          new DigestState(version, rootHash, store, settings)
        } else {
          val inVersion: VersionTag = VersionTag @@ store.lastVersionID.map(_.data).getOrElse(version)
          new DigestState(inVersion, rootHash, store, settings).update(version, rootHash).get //sync store
        }.ensuring(store.lastVersionID.get.data.sameElements(version))

      case (None, None) =>
        val version: VersionTag = VersionTag @@ store.lastVersionID.get.data
        val rootHash: Array[Byte] = store.get(ByteArrayWrapper(version)).get.data

        new DigestState(version, ADDigest @@ rootHash, store, settings)

      case _ => throw new Exception("Unsupported argument combination.")
    }
  }.getOrElse(EncryState.generateGenesisDigestState(dir, settings))
}