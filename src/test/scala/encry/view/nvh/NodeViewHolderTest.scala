//package encry.view.nvh
//
//import java.io.File
//
//import akka.actor.ActorSystem
//import akka.testkit.TestKit
//import com.typesafe.scalalogging.StrictLogging
//import encry.modifiers.InstanceFactory
//import encry.settings.EncryAppSettings
//import encry.storage.RootNodesStorage
//import encry.storage.VersionalStorage.{StorageKey, StorageValue, StorageVersion}
//import encry.storage.levelDb.versionalLevelDB.{LevelDbFactory, VLDBWrapper, VersionalLevelDBCompanion}
//import encry.utils.CoreTaggedTypes.VersionTag
//import encry.utils.FileHelper
//import encry.utils.implicits.UTXO.{combineAll, _}
//import encry.view.NodeViewHolder.ReceivableMessages.ModifierFromRemote
//import encry.view.state.UtxoState
//import encry.view.state.UtxoState.initialStateBoxes
//import encry.view.state.avlTree.AvlTree
//import encry.view.state.avlTree.utils.implicits.Instances._
//import org.encryfoundation.common.crypto.equihash.EquihashSolution
//import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
//import org.encryfoundation.common.modifiers.mempool.transaction.Transaction
//import org.encryfoundation.common.modifiers.state.box.AssetBox
//import org.encryfoundation.common.utils.Algos
//import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, Height}
//import org.iq80.leveldb.Options
//import org.scalatest.{BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike}
//import scorex.utils.Random
//
//class NodeViewHolderTest extends WordSpecLike
//  with BeforeAndAfterAll
//  with Matchers
//  with InstanceFactory
//  with OneInstancePerTest
//  with StrictLogging {
//
//  implicit val system: ActorSystem = ActorSystem("NodeViewHolderSpec")
//
//  override def afterAll: Unit = TestKit.shutdownActorSystem(system)
//
//  def genesis(stateDir: File, settings: EncryAppSettings): UtxoState = {
//    //check kind of storage
//    val levelDBInit = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
//    val storage = VLDBWrapper(VersionalLevelDBCompanion(levelDBInit, settings.levelDB.copy(keySize = 33), keySize = 33))
//    storage.insert(
//      StorageVersion @@ Array.fill(32)(0: Byte),
//      initialStateBoxes.map(bx => (StorageKey !@@ AvlTree.elementKey(bx.id), StorageValue @@ bx.bytes))
//    )
//    val rootsDir: File = FileHelper.getRandomTempDir
//    rootsDir.mkdir()
//    val rootStorage = {
//      val levelDBInit = LevelDbFactory.factory.open(rootsDir, new Options)
//      RootNodesStorage[StorageKey, StorageValue](levelDBInit, 3, rootsDir)
//    }
//
//    UtxoState(AvlTree[StorageKey, StorageValue](storage, rootStorage), Height @@ 0, settings.constants, None)
//  }
//
//  def generateGenesisBlockValidForState(state: UtxoState, tree: AvlTree[StorageKey, StorageValue]): (Block, AvlTree[StorageKey, StorageValue]) = {
//    val txs = List(coinbaseTransaction(0, privKey.publicImage))
//    val combinedStateChange: UtxoState.StateChange = combineAll(txs.map(UtxoState.tx2StateChange))
//    val insertTimestart = System.currentTimeMillis()
//    val newTree: AvlTree[StorageKey, StorageValue] = tree.insertAndDeleteMany(
//      StorageVersion @@ Random.randomBytes(),
//      combinedStateChange.outputsToDb.toList,
//      combinedStateChange.inputsToDb.toList,
//      Height @@ 0
//    )
//    val header = genHeader.copy(
//      parentId = Header.GenesisParentId,
//      transactionsRoot = Payload.rootHash(txs.map(_.id)),
//      height = settings.constants.GenesisHeight,
//      stateRoot = newTree.rootHash,
//      timestamp = System.currentTimeMillis()
//    )
//    Block(header, Payload(header.id, txs)) -> newTree
//  }
//
//  def generateNextBlockForStateWithSpendingAllPreviousBoxes(prevBlock: Block,
//                                                            state: UtxoState,
//                                                            box: Seq[AssetBox],
//                                                            splitCoef: Int = 2,
//                                                            addDiff: Difficulty = Difficulty @@ BigInt(0),
//                                                            tree: AvlTree[StorageKey, StorageValue]): (Block, AvlTree[StorageKey, StorageValue]) = {
//
//    val transactions: Seq[Transaction] = box.indices.foldLeft(box, Seq.empty[Transaction]) {
//      case ((boxes, transactionsL), _) =>
//        val tx: Transaction = defaultPaymentTransactionScratch(
//          privKey,
//          fee = 1,
//          timestamp = 11L,
//          useBoxes = IndexedSeq(boxes.head),
//          recipient = privKey.publicImage.address.address,
//          amount = boxes.head.amount - 1,
//          numOfOutputs = splitCoef
//        )
//        (boxes.tail, transactionsL :+ tx)
//    }._2.filter(tx => tx.inputs.forall(input => tree.contains(StorageKey @@ input.boxId))) ++ Seq(coinbaseTransaction(prevBlock.header.height + 1, privKey.publicImage))
//    val combinedStateChange: UtxoState.StateChange = combineAll(transactions.toList.map(UtxoState.tx2StateChange))
//    val insertTimestart = System.currentTimeMillis()
//    val prevRootHash = tree.rootNode.hash
//    val newTree: AvlTree[StorageKey, StorageValue] = tree.insertAndDeleteMany(
//      StorageVersion @@ Random.randomBytes(),
//      combinedStateChange.outputsToDb.toList,
//      combinedStateChange.inputsToDb.toList,
//      Height @@ (prevBlock.header.height + 1)
//    )
//    val header = Header(
//      1.toByte,
//      prevBlock.id,
//      Payload.rootHash(transactions.map(_.id)),
//      System.currentTimeMillis(),
//      prevBlock.header.height + 1,
//      prevBlock.header.timestamp + 1,
//      Difficulty @@ (BigInt(1) + addDiff),
//      EquihashSolution(Seq(1, 3)),
//      newTree.rootHash
//    )
//    logger.info(s"Creating block at height ${prevBlock.header.height + 1} with id: ${Algos.encode(header.id)}." +
//      s" Prev root: ${Algos.encode(prevRootHash)}. New: ${Algos.encode(newTree.rootNode.hash)}")
//    Block(header, Payload(header.id, transactions)) -> newTree
//  }
//
//  "nvh" should {
//    "not stock at rollback" in {
//
//      val tmpDirForNvh: File = FileHelper.getRandomTempDir
//      val tmpTree: AvlTree[StorageKey, StorageValue] = genesis(FileHelper.getRandomTempDir, settings).tree
//
//      val nvh = NVHUtils.initNvh(settings.copy(directory = tmpDirForNvh.getAbsolutePath))
//      val tmpDir: File = FileHelper.getRandomTempDir
//
//      val initialBoxes: IndexedSeq[AssetBox] = UtxoState.initialStateBoxes.toIndexedSeq
//      var state: UtxoState = genesis(tmpDir, settings)
//      val (genesisBlock: Block, treeToGen: AvlTree[StorageKey, StorageValue]) = generateGenesisBlockValidForState(state, tmpTree)
//
//      state = state.applyModifier(genesisBlock).right.get
//
//      //logger.info(s"digest root: ${Algos.encode(state.persistentProver.digest)}")
//
//      val stateGenerationResults: (List[(Block, Block)], Block, UtxoState, IndexedSeq[AssetBox], List[VersionTag], AvlTree[StorageKey, StorageValue]) =
//        (0 to 30).foldLeft(List.empty[(Block, Block)], genesisBlock, state, initialBoxes, List.empty[VersionTag], treeToGen) {
//          case ((blocks, block, stateL, boxes, versions, prevTree), i) =>
//            println(s"1. Before apply first block: ${Algos.encode(prevTree.avlStorage.currentVersion)}")
//            val (nextBlockMainChain: Block, newTree) = generateNextBlockForStateWithSpendingAllPreviousBoxes(
//              block,
//              stateL,
//              block.payload.txs.flatMap(_.newBoxes.map(_.asInstanceOf[AssetBox])).toIndexedSeq,
//              tree = prevTree
//            )
//            logger.info("Apply fork block")
//            val (nextBlockFork: Block, testTree) = generateNextBlockForStateWithSpendingAllPreviousBoxes(
//              block,
//              stateL,
//              block.payload.txs.flatMap(_.newBoxes.map(_.asInstanceOf[AssetBox])).toIndexedSeq,
//              addDiff = Difficulty @@ BigInt(100),
//              tree = prevTree.rollbackTo(newTree.avlStorage.versions.drop(2).last, blocks.map(_._1)).get
//            )
//            val newAnotherOnceTree = testTree.rollbackTo(testTree.avlStorage.versions.take(2).last, List.empty).get
//            val transactions: Seq[Transaction] = nextBlockMainChain.payload.txs
//            val combinedStateChange: UtxoState.StateChange = combineAll(transactions.toList.map(UtxoState.tx2StateChange))
//            val insertTimestart = System.currentTimeMillis()
//            val anotherNewTree: AvlTree[StorageKey, StorageValue] = newAnotherOnceTree.insertAndDeleteMany(
//              StorageVersion @@ Random.randomBytes(),
//              combinedStateChange.outputsToDb.toList,
//              combinedStateChange.inputsToDb.toList,
//              Height @@ (block.header.height + 1)
//            )
//            val stateRes = stateL.applyModifier(nextBlockMainChain)
//            logger.info(s"RES: ${stateRes}")
//            val stateN = stateRes.right.get
//            (blocks :+ (nextBlockMainChain, nextBlockFork),
//              nextBlockMainChain,
//              stateN,
//              boxes.drop(2),
//              versions :+ stateN.version,
//              anotherNewTree
//            )
//        }
//      val chain = List(genesisBlock) ++ stateGenerationResults._1.map(_._1)
//
////      val stateAfterRollback = stateGenerationResults._3.rollbackTo(stateGenerationResults._5.dropRight(1).last).get
////      val newState = stateAfterRollback.applyModifier(stateGenerationResults._1.last).right.get
////      val (newBlock: Block, avlTreeNew) = generateNextBlockForStateWithSpendingAllPreviousBoxes(
////        stateGenerationResults._1.last.,
////        newState,
////        stateGenerationResults._1.last.payload.txs.flatMap(_.newBoxes.map(_.asInstanceOf[AssetBox])).toIndexedSeq,
////        tree = stateGenerationResults._6
////      )
//      logger.info("==========================Start apply to nvh==========================")
//      println(s"genesisBlockHeader: ${genesisBlock.header}. Payload id: ${genesisBlock.payload.encodedId}")
//      println(s"nextBlock: ${chain(1).header}")
//
//      chain.dropRight(1).foreach{block =>
//        nvh ! ModifierFromRemote(block.header)
//        nvh ! ModifierFromRemote(block.payload)
//      }
//
////      chain.foreach{block =>
////        nvh ! ModifierFromRemote(block.payload)
////      }
//
//      val forkBlock = stateGenerationResults._1.dropRight(1).last._2
//
//      println(s"Fork header: ${forkBlock.header}")
//
//      nvh ! ModifierFromRemote(forkBlock.header)
//      nvh ! ModifierFromRemote(forkBlock.payload)
//
//      val anotherBlock = chain.last
//
//      nvh ! ModifierFromRemote(anotherBlock.header)
//      //nvh ! ModifierFromRemote(anotherBlock.payload)
//
//      //nvh.underlyingActor.nodeView.state.persistentProver.digest
//
////      nvh ! ModifierFromRemote(newBlock.header)
////      nvh ! ModifierFromRemote(newBlock.payload)
//
//      Thread.sleep(5000)
//    }
//  }
//}
