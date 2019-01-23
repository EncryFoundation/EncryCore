package encry.storage.levelDb.versionalLevelDB

import com.typesafe.scalalogging.StrictLogging
import encry.avltree._
import encry.avltree.benchmark.IODBBenchmark.getRandomTempDir
import encry.modifiers.InstanceFactory
import encry.modifiers.history.Block
import encry.modifiers.state.StateModifierSerializer
import encry.modifiers.state.box.AssetBox
import encry.utils.{EncryGenerator, FileHelper}
import io.iohk.iodb.LSMStore
import org.encryfoundation.common.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADValue
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.hash.{Blake2b256, Digest32}

class WalletVersionalLevelDBTest extends PropSpec with Matchers with EncryGenerator with InstanceFactory with StrictLogging {

  type HF = Blake2b256.type
  implicit val hf: HF = Blake2b256

  def amountInBlocks(blocks: Seq[Block], prover: encry.avltree.PersistentBatchAVLProver[Digest32, HF]): Long = {
    val toAdd = blocks.map(_.payload.transactions.map(_.newBoxes.collect { case ab: AssetBox => ab.amount }.sum).sum).sum
    val toRemove = blocks.map(
        _.payload.transactions.map(
          _.inputs.map(input =>
            prover.unauthenticatedLookup(input.boxId)
              .map(bytes => StateModifierSerializer.parseBytes(bytes, input.boxId.head)
                .collect { case ab: AssetBox => ab.amount }).map(_.get).sum
          ).sum
        ).sum
      ).sum
    toAdd - toRemove
  }


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

    //init wallet tree

    val walletVersionalDB = WalletVersionalLevelDB(db)

    //add blocks to wallet

    blocksToWallet.foreach(walletVersionalDB.add)

    //check correct balance

    walletVersionalDB.getBalances.head._2 shouldEqual correctBalance
  }

  property("Applying 35 blocks. Restart levelDB. Balance should be the same ") {

    val blocksToWallet: Seq[Block] = (0 until 35).foldLeft(generateDummyHistory, Seq.empty[Block]) {
      case ((prevHistory, blocks), _) =>
        val block: Block = generateNextBlock(prevHistory)
        prevHistory.append(block.header).get._1.append(block.payload).get._1.reportModifierIsValid(block) -> (blocks :+ block)
    }._2

    val dir = FileHelper.getRandomTempDir
    if (!dir.exists()) dir.mkdirs()

    val db: DB = LevelDbFactory.factory.open(dir, new Options)

    val correctBalance = blocksToWallet.foldLeft(0: Long) {
      case (balance, block) =>
        val newBalance = balance + block.payload.transactions.map(_.newBoxes.map(_.asInstanceOf[AssetBox].amount).sum).sum
        logger.info(s"After applying block: ${Algos.encode(block.id)} wallet should be: $newBalance")
        newBalance
    }

    //init wallet tree

    val walletVersionalDB = WalletVersionalLevelDB(db)

    //add blocks to wallet

    blocksToWallet.foreach(walletVersionalDB.add)

    //check correct balance

    walletVersionalDB.getBalances.head._2 shouldEqual correctBalance

    //close db

    walletVersionalDB.close()

    //restart

    val newDb: DB = LevelDbFactory.factory.open(dir, new Options)

    val newWalletVersionalDB = WalletVersionalLevelDB(newDb)

    //check balance

    newWalletVersionalDB.getBalances.head._2 shouldEqual correctBalance
  }


  property("Rollback of 10 block. Only coinbase txs") {

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

    val db: DB = {
      val dir = FileHelper.getRandomTempDir
      if (!dir.exists()) dir.mkdirs()
      LevelDbFactory.factory.open(dir, new Options)
    }

    //init ver db
    val walletVersionalDB = WalletVersionalLevelDB(db)

    //add all blocks
    blocksToWallet.foreach(walletVersionalDB.add)

    //Check params before rollback

      //check correct balance
      blocksToWallet.last.id shouldEqual walletVersionalDB.id

      //check correct head of tree
      amountInBlocks(blocksToWallet) shouldEqual walletVersionalDB.getBalances.head._2

    // do rollback

    val result = walletVersionalDB.rollbackTo(blocksToWallet.dropRight(10).last.id, persistentProver)

    // Check params after rollback

      //check correct balance
      amountInBlocks(blocksToWallet.dropRight(10)) shouldEqual walletVersionalDB.getBalances.head._2

      //check correct head of tree
      blocksToWallet.dropRight(10).last.id shouldEqual walletVersionalDB.id
  }

  property("Rollback of 10 blocks. With spending txs") {

    val rollbackLength = 10

    val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
    val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(new LSMStore(getRandomTempDir), np)
    val persistentProver: PersistentBatchAVLProver[Digest32, HF] =
      PersistentBatchAVLProver.create(
        new BatchAVLProver[Digest32, HF](
          keyLength = 32, valueLengthOpt = None), storage).get

    val blocksToWallet: Seq[Block] = generateFakeChain(25)

    def applyModifications(mods: Seq[Modification]): Unit =
      mods.foreach(m => {
        persistentProver.performOneOperation(m).ensuring(_.isSuccess, "Mod application failed.")
      })

    def modificationsFromBlocks(blocks: Seq[Block]): Seq[Modification] =
      blocks
        .flatMap(_.payload.transactions
          .flatMap(tx =>
            tx.inputs.map(input => Remove(input.boxId)) ++ tx.newBoxes.map(box => Insert(box.id, ADValue @@ box.bytes))
          )
        ).distinct

    applyModifications(modificationsFromBlocks(blocksToWallet.dropRight(rollbackLength)))

    persistentProver.generateProofAndUpdateStorage()

    val amountAfterRollback = amountInBlocks(blocksToWallet.dropRight(rollbackLength), persistentProver)

    val ppRoot = persistentProver.digest

    applyModifications(modificationsFromBlocks(blocksToWallet.takeRight(rollbackLength)))

    persistentProver.generateProofAndUpdateStorage()

    val db: DB = {
      val dir = FileHelper.getRandomTempDir
      if (!dir.exists()) dir.mkdirs()
      LevelDbFactory.factory.open(dir, new Options)
    }

    //init ver db
    val walletVersionalDB = WalletVersionalLevelDB(db)

    //add all blocks
    blocksToWallet.foreach(walletVersionalDB.add)

    //Check params before rollback

      //check correct head of tree
      blocksToWallet.last.id shouldEqual walletVersionalDB.id

      //check correct balance
      amountInBlocks(blocksToWallet, persistentProver) shouldEqual walletVersionalDB.getBalances.head._2

    // do rollback

    persistentProver.rollback(ppRoot)

    walletVersionalDB.rollbackTo(blocksToWallet.dropRight(rollbackLength).last.id, persistentProver)

    // Check params after rollback

      //check correct balance
      amountAfterRollback shouldEqual walletVersionalDB.getBalances.head._2

      //check correct head of tree
      blocksToWallet.dropRight(rollbackLength).last.id shouldEqual walletVersionalDB.id
  }

  property("Rollback of 10 blocks then add 5 blocks. With spending txs.") {

    val rollbackLength = 10

    val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)
    val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(new LSMStore(getRandomTempDir), np)
    val persistentProver: PersistentBatchAVLProver[Digest32, HF] =
      PersistentBatchAVLProver.create(
        new BatchAVLProver[Digest32, HF](
          keyLength = 32, valueLengthOpt = None), storage).get

    val blocksToWallet: Seq[Block] = generateFakeChain(25)

    def applyModifications(mods: Seq[Modification]): Unit =
      mods.foreach(m => {
        persistentProver.performOneOperation(m).ensuring(_.isSuccess, "Mod application failed.")
      })

    def modificationsFromBlocks(blocks: Seq[Block]): Seq[Modification] =
      blocks
        .flatMap(_.payload.transactions
          .flatMap(tx =>
            tx.inputs.map(input => Remove(input.boxId)) ++ tx.newBoxes.map(box => Insert(box.id, ADValue @@ box.bytes))
          )
        ).distinct

    applyModifications(modificationsFromBlocks(blocksToWallet.dropRight(rollbackLength)))

    persistentProver.generateProofAndUpdateStorage()

    val amountAfterRollback = amountInBlocks(blocksToWallet.dropRight(rollbackLength), persistentProver)

    val ppRoot = persistentProver.digest

    applyModifications(modificationsFromBlocks(blocksToWallet.takeRight(rollbackLength)))

    persistentProver.generateProofAndUpdateStorage()

    val db: DB = {
      val dir = FileHelper.getRandomTempDir
      if (!dir.exists()) dir.mkdirs()
      LevelDbFactory.factory.open(dir, new Options)
    }

    //init ver db
    val walletVersionalDB = WalletVersionalLevelDB(db)

    //add all blocks
    blocksToWallet.foreach(walletVersionalDB.add)

    //Check params before rollback

      //check correct balance
      blocksToWallet.last.id shouldEqual walletVersionalDB.id

      //check correct head of tree
      amountInBlocks(blocksToWallet, persistentProver) shouldEqual walletVersionalDB.getBalances.head._2

    // do rollback

    persistentProver.rollback(ppRoot)

    walletVersionalDB.rollbackTo(blocksToWallet.dropRight(rollbackLength).last.id, persistentProver)

    // Check params after rollback

      //check correct balance
      amountAfterRollback shouldEqual walletVersionalDB.getBalances.head._2

    //check correct head of tree
      blocksToWallet.dropRight(rollbackLength).last.id shouldEqual walletVersionalDB.id

    // add 5 blocks

    val modificationOf5Blocks = modificationsFromBlocks(
      blocksToWallet.slice(blocksToWallet.length - rollbackLength, blocksToWallet.length - rollbackLength + 5)
    )

    applyModifications(modificationOf5Blocks)

    persistentProver.generateProofAndUpdateStorage()

    blocksToWallet.slice(blocksToWallet.length - rollbackLength, blocksToWallet.length - rollbackLength + 5).foreach(walletVersionalDB.add)

    //check balance

      amountInBlocks(blocksToWallet.take(blocksToWallet.length - rollbackLength + 5),
        persistentProver) shouldEqual walletVersionalDB.getBalances.head._2

  }
}
