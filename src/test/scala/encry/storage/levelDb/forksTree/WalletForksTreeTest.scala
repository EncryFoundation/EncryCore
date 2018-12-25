package encry.storage.levelDb.forksTree

import com.typesafe.scalalogging.StrictLogging
import encry.avltree._
import encry.avltree.benchmark.IODBBenchmark.getRandomTempDir
import encry.modifiers.InstanceFactory
import encry.modifiers.history.Block
import encry.modifiers.state.box.AssetBox
import encry.utils.{EncryGenerator, FileHelper}
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADValue
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.{Blake2b256, Digest32}

import scala.util.Success

class WalletForksTreeTest extends PropSpec with Matchers with EncryGenerator with InstanceFactory with StrictLogging{

  property("Applying 35 blocks. Wallet balance should increase") {

    val blocksToWallet: Seq[Block] = (0 until 35).foldLeft(generateDummyHistory, Seq.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block) -> (blocks :+ block)
    }._2

    val db: DB = {
      val dir = FileHelper.getRandomTempDir
      if (!dir.exists()) dir.mkdirs()
      LevelDbFactory.factory.open(dir, new Options)
    }

    val correctBalance = blocksToWallet.foldLeft(0: Long) {
      case (balance, block) =>
        val newBalance = balance + block.payload.transactions.map(_.newBoxes.map(_.asInstanceOf[AssetBox].amount).sum).sum
        logger.info(s"After applying block: ${Algos.encode(block.id)} wallet should be: $newBalance")
        newBalance
    }

    val walletTree = WalletForksTree(db)

    blocksToWallet.foreach(walletTree.add)

    walletTree.getBalances.head._2 shouldEqual correctBalance
  }


  property("Rollback of 10 block. Only coinbase txs") {

    type HF = Blake2b256.type
    implicit val hf: HF = Blake2b256

    val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
    val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(new LSMStore(getRandomTempDir), np)
    val persistentProver: PersistentBatchAVLProver[Digest32, HF] =
    PersistentBatchAVLProver.create(
      new BatchAVLProver[Digest32, HF](
        keyLength = 32, valueLengthOpt = None), storage).get

    val blocksToWallet: Seq[Block] = (0 until 35).foldLeft(generateDummyHistory, Seq.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory, txsQty = 0)
        prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block) -> (blocks :+ block)
    }._2

    def amountInBlocks(blocks: Seq[Block]): Long =
      blocks.map(_.payload.transactions.map(_.newBoxes.collect{case ab: AssetBox => ab.amount}.sum).sum).sum

    def applyModifications(mods: Seq[Modification]): Unit =
      mods.foreach(m => {
        persistentProver.performOneOperation(m).ensuring(_.isSuccess, "Mod application failed.")
      })

    val modificationsFromBlocks =
      blocksToWallet
        .flatMap(_.payload.transactions
          .flatMap(_.newBoxes.map(box => Insert(box.id, ADValue @@ box.bytes)))).distinct

    applyModifications(modificationsFromBlocks)

    persistentProver.generateProofAndUpdateStorage()

    val db: DB = {
      val dir = FileHelper.getRandomTempDir
      if (!dir.exists()) dir.mkdirs()
      LevelDbFactory.factory.open(dir, new Options)
    }

    //init tree
    val walletTree = WalletForksTree(db)

    //add all blocks
    blocksToWallet.foreach(walletTree.add)

    //Check params before rollback

      //check correct balance
      blocksToWallet.last.id shouldEqual walletTree.id

      //check correct head of tree
      amountInBlocks(blocksToWallet) shouldEqual walletTree.getBalances.head._2

    // do rollback

    val result = walletTree.rollbackTo(blocksToWallet.dropRight(10).last.id, persistentProver)

    // Check params after rollback

      //check correct balance
      amountInBlocks(blocksToWallet.dropRight(10)) shouldEqual walletTree.getBalances.head._2

      //check correct head of tree
      blocksToWallet.dropRight(10).last.id shouldEqual walletTree.id
  }

}
