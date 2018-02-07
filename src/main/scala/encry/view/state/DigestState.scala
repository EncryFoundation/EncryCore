package encry.view.state

import java.io.File

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.ADProofs
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.{Algos, Constants, NodeSettings}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.VersionTag
import scorex.core.transaction.state.ModifierValidation
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest

import scala.util.{Failure, Success, Try}

/**
  * Minimal state variant which is storing only digest of UTXO authenticated as a dynamic dictionary.
  */
class DigestState protected(override val version: VersionTag,
                            override val rootHash: ADDigest,
                            val store: Store,
                            settings: NodeSettings)
  extends EncryState[DigestState]
    with ModifierValidation[EncryPersistentModifier]
    with ScorexLogging {

  store.lastVersionID
    .foreach(id => assert(version sameElements id.data, "`version` should always be equal to store.lastVersionID"))

  override val maxRollbackDepth = 10

  def validate(mod: EncryPersistentModifier): Try[Unit] = mod match {
    case block: EncryBlock =>
      Try {
        if (!ADProofs.proofDigest(block.adProofsOpt.get.proofBytes).sameElements(block.header.adProofsRoot))
          Failure(new Error(s"Got invalid Proof for block: $block"))

        val txs = block.payload.transactions
        val declaredHash = block.header.stateRoot

        txs.foldLeft(Success(): Try[Unit]) { case (status, tx) =>
          status.flatMap(_ => tx.semanticValidity)
        }.flatMap(_ => block.adProofsOpt.map(_.verify(getAllStateChanges(txs), rootHash, declaredHash))
          .getOrElse(Failure(new Error("Proofs are empty"))))
      }.flatten match {
        case s: Success[_] =>
          log.info(s"Valid modifier applied to DigestState: ${block.encodedId}")
          s
        case Failure(e) =>
          log.warn(s"Modifier $mod is not valid: ", e)
          Failure(e)
      }
    case mod: Any =>
      Failure(new Error(s"Unhandled modifier: $mod"))
  }

  private def update(newVersion: VersionTag, newRootHash: ADDigest): Try[DigestState] = Try {
    val wrappedVersion = ByteArrayWrapper(newVersion)
    store.update(wrappedVersion, toRemove = Seq(), toUpdate = Seq(wrappedVersion -> ByteArrayWrapper(newRootHash)))
    new DigestState(newVersion, newRootHash, store, settings)
  }

  //todo: utxo snapshot could go here
  override def applyModifier(mod: EncryPersistentModifier): Try[DigestState] = mod match {
    case block: EncryBlock if settings.verifyTransactions =>
      log.info(s"Got new full block with id ${block.encodedId} with root ${Algos.encoder.encode(block.header.stateRoot)}")
      this.validate(block).flatMap(_ => update(VersionTag @@ block.header.id, block.header.stateRoot))

    case header: EncryBlockHeader if !settings.verifyTransactions =>
      log.info(s"Got new Header ${header.encodedId} with root ${Algos.encoder.encode(header.stateRoot)}")
      update(VersionTag @@ header.id, header.stateRoot)

    case a: Any =>
      log.info(s"Unhandled modifier: $a")
      Failure(new Error(s"Unhandled modifier: $mod"))
  }

  override def rollbackTo(version: VersionTag): Try[DigestState] = {
    log.info(s"Rollback Digest State to version ${Algos.encoder.encode(version)}")
    val wrappedVersion = ByteArrayWrapper(version)
    Try(store.rollback(wrappedVersion)).map { _ =>
      store.clean(Constants.keepVersions)
      val rootHash = ADDigest @@ store.get(wrappedVersion).get.data
      log.info(s"Rollback to version ${Algos.encoder.encode(version)} with roothash ${Algos.encoder.encode(rootHash)}")
      new DigestState(version, rootHash, store, settings)
    }
  }

  override def rollbackVersions: Iterable[VersionTag] = store.rollbackVersions().map(VersionTag @@ _.data)

  def closeStorage(): Unit = store.close()

}

object DigestState {

  def create(versionOpt: Option[VersionTag],
             rootHashOpt: Option[ADDigest],
             dir: File,
             settings: NodeSettings): Try[DigestState] = Try {
    val store = new LSMStore(dir, keepVersions = Constants.keepVersions)

    (versionOpt, rootHashOpt) match {

      case (Some(version), Some(rootHash)) =>
        if (store.lastVersionID.isDefined && store.lastVersionID.forall(_.data sameElements version)) {
          new DigestState(version, rootHash, store, settings)
        } else {
          val inVersion = VersionTag @@ store.lastVersionID.map(_.data).getOrElse(version)
          new DigestState(inVersion, rootHash, store, settings).update(version, rootHash).get //sync store
        }.ensuring(store.lastVersionID.get.data.sameElements(version))

      case (None, None) =>
        val version = VersionTag @@ store.lastVersionID.get.data
        val rootHash = store.get(ByteArrayWrapper(version)).get.data

        new DigestState(version, ADDigest @@ rootHash, store, settings)

      case _ => throw new Error("Unsupported argument combination.")
    }
  }
}
