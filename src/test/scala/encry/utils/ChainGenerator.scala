package encry.utils

import java.io.File

import akka.actor.ActorRef
import encry.modifiers.mempool.TransactionFactory
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageType, StorageValue, StorageVersion}
import encry.storage.iodb.versionalIODB.IODBWrapper
import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
import encry.view.state.{BoxHolder, UtxoState}
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.crypto.{PrivateKey25519, PublicKey25519, Signature25519}
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.modifiers.mempool.directive.TransferDirective
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.modifiers.mempool.transaction.{Input, Pay2PubKeyAddress, Proof, PubKeyLockedContract, Transaction, UnsignedTransaction}
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.{AssetBox, MonetaryBox}
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, Difficulty, Height}
import org.encryfoundation.prismlang.core.wrapped.BoxedValue
import org.iq80.leveldb.Options
import scorex.crypto.hash.Digest32
import scorex.crypto.signatures.PublicKey

import scala.collection.Seq
import scorex.utils.{Random => ScorexRandom}

import scala.util.Random

object ChainGenerator {

  def genChain(privKey: PrivateKey25519, dir: File, settings: EncryAppSettings, blockQty: Int, transPerBlock: Int = 1,
               genInvalidBlockFrom: Option[Int] = None): (UtxoState, UtxoState, List[Block]) = {

    def utxoFromBoxHolder(bh: BoxHolder, dir: File, nodeViewHolderRef: Option[ActorRef], settings: EncryAppSettings,
                          storageType: StorageType): UtxoState = {
      val storage = settings.storage.state match {
        case VersionalStorage.IODB =>
          IODBWrapper(new LSMStore(dir, keepVersions = settings.constants.DefaultKeepVersions))
        case VersionalStorage.LevelDB =>
          val levelDBInit = LevelDbFactory.factory.open(dir, new Options)
          VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB, keySize = 32))
      }

      storage.insert(
        StorageVersion @@ Array.fill(32)(0: Byte),
        bh.boxes.values.map(bx => (StorageKey !@@ bx.id, StorageValue @@ bx.bytes)).toList
      )

      new UtxoState(storage, settings.constants)
    }

    def paymentTransactionWithMultipleOutputs(privKey: PrivateKey25519, fee: Amount, timestamp: Long, useBoxes: IndexedSeq[MonetaryBox],
                                              recipient: Address, amount: Amount, tokenIdOpt: Option[ADKey] = None,
                                              numOfOutputs: Int): Transaction = {

      val pubKey: PublicKey25519 = privKey.publicImage
      val uInputs: IndexedSeq[Input] = useBoxes
        .map(bx => Input.unsigned(bx.id, Right(PubKeyLockedContract(pubKey.pubKeyBytes))))
        .toIndexedSeq

      val change: Amount = useBoxes.map(_.amount).sum - (amount + fee)
      val directives: IndexedSeq[TransferDirective] =
        if (change > 0) TransferDirective(recipient, amount, tokenIdOpt) +: (0 until numOfOutputs).map(_ =>
          TransferDirective(pubKey.address.address, change / numOfOutputs, tokenIdOpt))
        else IndexedSeq(TransferDirective(recipient, amount, tokenIdOpt))

      val uTransaction: UnsignedTransaction = UnsignedTransaction(fee, timestamp, uInputs, directives)
      val signature: Signature25519 = privKey.sign(uTransaction.messageToSign)
      uTransaction.toSigned(IndexedSeq.empty, Some(Proof(BoxedValue.Signature25519Value(signature.bytes.toList))))
    }

    def genForkBlock(boxQty: Int, prevBlock: Block, box: AssetBox, recipient: Address,
                     addDiff: Difficulty = Difficulty @@ BigInt(0)): (Block, Seq[AssetBox]) = {
      val timestamp = System.currentTimeMillis()

      val transactions: Seq[Transaction] = Seq(
        paymentTransactionWithMultipleOutputs(privKey, 1L, timestamp, IndexedSeq(box), recipient, 1000L, None, boxQty),
        coinbaseTransaction(prevBlock.header.height + 1)
      )

      val header = Header(1.toByte, prevBlock.id, Payload.rootHash(transactions.map(_.id)), timestamp,
        prevBlock.header.height + 1, Random.nextLong(), Difficulty @@ (BigInt(1) + addDiff), EquihashSolution(Seq(1, 3)))

      (Block(header, Payload(header.id, transactions)), transactions.head.newBoxes.tail.map(_.asInstanceOf[AssetBox]).toSeq)
    }

    def generateGenesisBlock(genesisHeight: Height): Block = {
      val coinbaseTrans = TransactionFactory.coinbaseTransactionScratch(
        privKey.publicImage,
        System.currentTimeMillis(),
        settings.constants.InitialEmissionAmount,
        amount = 0L,
        height = genesisHeight
      )

      val txs: Seq[Transaction] = Seq(coinbaseTrans)
      val txsRoot: Digest32 = Payload.rootHash(txs.map(_.id))
      val header =
        Header(
          1.toByte,
          Header.GenesisParentId,
          txsRoot,
          System.currentTimeMillis(),
          genesisHeight,
          Random.nextLong(),
          settings.constants.InitialDifficulty,
          EquihashSolution(Seq(1, 3))
        )
      Block(header, Payload(header.id, txs))
    }

    def coinbaseTransaction(height: Int): Transaction = TransactionFactory.coinbaseTransactionScratch(
      privKey.publicImage,
      System.currentTimeMillis(),
      supply = settings.constants.InitialEmissionAmount,
      amount = 1L,
      height = Height @@ height
    )

    def genNextBlockForState(prevBlock: Block, state: UtxoState, boxes: Seq[AssetBox], recipient: Address, invalid: Boolean = false,
                             addDiff: Difficulty = Difficulty @@ BigInt(0)): (Block, Seq[AssetBox]) = {
      val timestamp = System.currentTimeMillis()

      val amount = if (invalid) boxes.map(_.amount).sum + 1L else 1L

      val transactions: Seq[Transaction] =
        boxes.map(b => TransactionFactory.defaultPaymentTransactionScratch(privKey, 1L, timestamp, IndexedSeq(b), recipient, amount)) :+
          coinbaseTransaction(prevBlock.header.height + 1)

      val header = Header(1.toByte, prevBlock.id, Payload.rootHash(transactions.map(_.id)), timestamp,
        prevBlock.header.height + 1, Random.nextLong(), Difficulty @@ (BigInt(1) + addDiff), EquihashSolution(Seq(1, 3)))

      val newBoxes = transactions.filter(_.newBoxes.size > 1).map(_.newBoxes.toSeq(1).asInstanceOf[AssetBox])
      (Block(header, Payload(header.id, transactions)), newBoxes)
    }

    def randomAddress: Address = Pay2PubKeyAddress(PublicKey @@ ScorexRandom.randomBytes()).address

    assert(blockQty >= 2, "chain at least 2 blocks")

    val genesisBlock: Block = generateGenesisBlock(Height @@ 0)

    val state: UtxoState = utxoFromBoxHolder(BoxHolder(Seq.empty), dir, None, settings, VersionalStorage.LevelDB)
      .applyModifier(genesisBlock).right.get

    val (forkBlock, forkBoxes) =
      genForkBlock(transPerBlock, genesisBlock, genesisBlock.payload.txs.head.newBoxes.map(_.asInstanceOf[AssetBox]).head, randomAddress)
    val forkState = state.applyModifier(forkBlock).right.get

    val (chain, _, newState, _) =
      (2 until blockQty).foldLeft(List(genesisBlock, forkBlock), forkBlock, forkState, forkBoxes) {
        case ((blocks, blockL, stateL, boxes), height) =>
          val invalid = genInvalidBlockFrom.exists(height + 1 >= _)
          val (newBlock, newBoxes) = genNextBlockForState(blockL, stateL, boxes, randomAddress, invalid)
          val newState = if (invalid) stateL else stateL.applyModifier(newBlock).right.get
          (blocks :+ newBlock, newBlock, newState, newBoxes)
      }
    (state, newState, chain)
  }

}