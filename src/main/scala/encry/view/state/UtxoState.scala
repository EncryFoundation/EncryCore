package encry.view.state

import java.io.File

import akka.actor.ActorRef
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.mempool.{EncryBaseTransaction, EncryPaymentTransaction}
import encry.modifiers.state.box._
import encry.modifiers.state.TransactionValidator
import encry.modifiers.state.box.body.BaseBoxBody
import encry.modifiers.state.box.proposition.AddressProposition
import encry.settings.Algos
import io.iohk.iodb.{ByteArrayWrapper, LSMStore, Store}
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.{EphemerealNodeViewModifier, VersionTag}
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.authds.avltree.batch.{BatchAVLProver, NodeParameters, PersistentBatchAVLProver, VersionedIODBAVLStorage}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

import scala.util.{Failure, Success, Try}

class UtxoState(override val version: VersionTag,
                val store: Store,
                /*nodeViewHolderRef: Option[ActorRef]*/)
  extends EncryBaseState[UtxoState] with TransactionValidator with ScorexLogging {

  import UtxoState.metadata

  implicit val hf = new Blake2b256Unsafe
  private lazy val np = NodeParameters(keySize = 32, valueSize = EncryPaymentBoxSerializer.Length, labelSize = 32)
  protected lazy val storage = new VersionedIODBAVLStorage(store, np)

  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
    PersistentBatchAVLProver.create(
      new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32,
        valueLengthOpt = Some(EncryPaymentBoxSerializer.Length)),
      storage,
    ).get


  // TODO: Why 10?
  override def maxRollbackDepth: Int = 10

  // TODO: Fix return type
  def getClosedBox(boxType: Any, boxId: Array[Byte]): Option[Box[_]] = {
    boxType match {
      case _: EncryPaymentBox => store.get(ByteArrayWrapper(boxId))
        .map(_.data)
        .map(EncryPaymentBoxSerializer.parseBytes)
        .flatMap(_.toOption)
    }
  }

  def boxChanges(txs: Seq[EncryBaseTransaction]): EncryBoxStateChanges = {
    // Use neither `.filter` nor any validity checks here!
    // This method should be invoked when all txs are already validated.
    EncryBoxStateChanges(
      txs.flatMap { tx =>
        tx match {
          case tx: EncryPaymentTransaction =>
            tx.unlockers.map( unl => Removal(unl.closedBoxId)) ++
            tx.newBoxes.map( bx => Insertion(bx) )

//        case tx: AnotherTypeTransaction => ...
      }
    })
  }

  private[state] def checkTransactions(txs: Seq[EncryBaseTransaction]) = Try {
    // TODO: Достать выходы из бд и проверить соответствие адресов публ. ключам.
//    txs.foreach(tx => tx.semanticValidity.get)

    txs.forall {
      case tx: EncryPaymentTransaction => {
        println(s"TRX SemValid = ${tx.semanticValidity.isSuccess}")
        // println(s"TRX input valid = ${!(tx.useOutputs.forall(key => store.get(new ByteArrayWrapper(key)).isEmpty))}")
        tx.semanticValidity.isSuccess &&
          tx.useOutputs.forall(key =>
            store.get(new ByteArrayWrapper(key)) match {
              case Some(data) => {
                data match {
                  case data: Store.V => {
                    EncryPaymentBoxSerializer.parseBytes(data.data) match {
                      case nb: Try[EncryPaymentBox] => {
                        println(s"in nb = ${nb.get.proposition.address}")
                        println(s"in tx = ${tx.sender.address}")
                        println("trx accept" + (nb.get.proposition.address == tx.sender.address))
                        nb.get.proposition.address == tx.sender.address
                      }
                      case _ => {
                        println("1 fail")
                        false
                      }
                    }
                  }
                  case _ => {
                    println("2 fail")
                    false
                  }
                }
              }
              case None => {
                println("3 fail")
                false
              }
            }
            //
          )
      }
    }

  }

  // Dispatches applying `Modifier` of particular type.
  override def applyModifier(mod: EncryPersistentModifier): Try[UtxoState] = mod match {
    case block: EncryBlock =>
      log.debug(s"Applying block with header ${block.header.encodedId} to UtxoState with " +
        s"root hash ${Algos.encode(rootHash)}")

      checkTransactions(block.payload.transactions) match {
        case Success(_) =>
          Try {
            // TODO: Пробуем применить изменения к `store`.
            println("Modifier applied")
            new UtxoState(VersionTag @@ block.id, store /*, nodeViewHolderRef*/)
          }
        case Failure(e) =>
          Failure(e)
      }
  }

  //TODO: implement
  override def rollbackTo(version: VersionTag): Try[UtxoState] = Try{this}
  //TODO: implement
  override def rollbackVersions: Iterable[VersionTag] = List(VersionTag @@ "test".getBytes())
  //TODO: implement
  override lazy val rootHash: ADDigest = ADDigest @@ "test".getBytes()
  //TODO: implement
  override def validate(tx: EphemerealNodeViewModifier): Try[Unit] = Try{
    tx match {
      case tx: EncryPaymentTransaction =>
        tx.semanticValidity.isSuccess && tx.useOutputs.forall(key => store.get(new ByteArrayWrapper(key)).isEmpty)
    }
  }

}

object UtxoState {

  private lazy val bestVersionKey = Algos.hash("best state version") // TODO: ???

  def create(dir: File): UtxoState = {
    val store = new LSMStore(dir, keepVersions = 20) // todo: magic number, move to settings
    val dbVersion = store.get(ByteArrayWrapper(bestVersionKey)).map( _.data)
    new UtxoState(VersionTag @@ dbVersion.getOrElse(EncryBaseState.genesisStateVersion), store)
  }

  private def metadata(modId: VersionTag, stateRoot: ADDigest): Seq[(Array[Byte], Array[Byte])] = {
    val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modId -> stateRoot
    val stateDigestIdIdxElem = Algos.hash(stateRoot) -> modId
    val bestVersion = bestVersionKey -> modId

    Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion)
  }
}
