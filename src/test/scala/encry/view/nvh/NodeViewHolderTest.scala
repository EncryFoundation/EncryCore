package encry.view.nvh

//class NodeViewHolderTest extends WordSpecLike
//  with BeforeAndAfterAll
//  with Matchers
//  with InstanceFactory
//  with OneInstancePerTest {
//
//  implicit val system: ActorSystem = ActorSystem("NodeViewHolderSpec")
//  val settings: EncryAppSettings = DummyEncryAppSettingsReader.read
//
//  override def afterAll: Unit = TestKit.shutdownActorSystem(system)
//
//  "nvh" should {
//    "not stock at rollback" in {
//
//      val tmpDirForNvh: File = getRandomTempDir
//
//      val nvh = NVHUtils.initNvh(settings.copy(directory = tmpDirForNvh.getAbsolutePath))
//      val tmpDir: File = getRandomTempDir
//
//      val initialBoxes: IndexedSeq[AssetBox] = EncryState.initialStateBoxes.toIndexedSeq
//      val boxesHolder: BoxHolder = BoxHolder(initialBoxes)
//      var state: UtxoState = utxoFromBoxHolder(boxesHolder, tmpDir, None, settings, VersionalStorage.IODB)
//      val genesisBlock: Block = generateGenesisBlockValidForState(state)
//
//      state = state.applyModifier(genesisBlock).get
//
//      //logger.info(s"digest root: ${Algos.encode(state.persistentProver.digest)}")
//
//      val stateGenerationResults: (List[(Block, Block)], Block, UtxoState, IndexedSeq[AssetBox], List[VersionTag]) =
//        (0 to 400).foldLeft(List.empty[(Block, Block)], genesisBlock, state, initialBoxes, List.empty[VersionTag]) {
//          case ((blocks, block, stateL, boxes, versions), i) =>
//            val nextBlockMainChain: Block = generateNextBlockForStateWithSpendingAllPreviousBoxes(
//              block,
//              stateL,
//              block.payload.txs.flatMap(_.newBoxes.map(_.asInstanceOf[AssetBox])).toIndexedSeq,
//              isLast = i == 400
//            )
//            val nextBlockFork: Block = generateNextBlockForStateWithSpendingAllPreviousBoxes(
//              block,
//              stateL,
//              block.payload.txs.flatMap(_.newBoxes.map(_.asInstanceOf[AssetBox])).toIndexedSeq,
//              addDiff = Difficulty @@ BigInt(100),
//              isLast = i == 400
//            )
//            val stateN: UtxoState = stateL.applyModifier(nextBlockMainChain).get
//            (blocks :+ (nextBlockMainChain, nextBlockFork),
//              nextBlockMainChain,
//              stateN,
//              boxes.drop(2),
//              versions :+ stateN.version
//            )
//        }
//      val chain: List[Block] = genesisBlock +: stateGenerationResults._1.map(_._1)
//
//      val stateAfterRollback = stateGenerationResults._3.rollbackTo(stateGenerationResults._5.dropRight(1).last).get
//      val newState = stateAfterRollback.applyModifier(stateGenerationResults._1.last._2).get
//      val newBlock: Block = generateNextBlockForStateWithSpendingAllPreviousBoxes(
//        stateGenerationResults._1.last._2,
//        newState,
//        stateGenerationResults._1.last._2.payload.txs.flatMap(_.newBoxes.map(_.asInstanceOf[AssetBox])).toIndexedSeq
//      )
//
//      println(s"genesisBlockHeader: ${genesisBlock.header}. Payload id: ${genesisBlock.payload.encodedId}")
//      println(s"nextBlock: ${chain(1).header}")
//
//      chain.foreach{block =>
//        nvh ! ModifiersFromRemote(Header.modifierTypeId, Seq(HeaderProtoSerializer.toProto(block.header).toByteArray))
//        //nvh ! ModifiersFromRemote(Payload.modifierTypeId, Seq(PayloadProtoSerializer.toProto(block.payload).toByteArray))
//      }
//
//      chain.foreach{block =>
//        nvh ! ModifiersFromRemote(Payload.modifierTypeId, Seq(PayloadProtoSerializer.toProto(block.payload).toByteArray))
//      }
//
//      val forkBlock = stateGenerationResults._1.last._2
//
//      nvh ! ModifiersFromRemote(Header.modifierTypeId, Seq(HeaderProtoSerializer.toProto(forkBlock.header).toByteArray))
//      nvh ! ModifiersFromRemote(Payload.modifierTypeId, Seq(PayloadProtoSerializer.toProto(forkBlock.payload).toByteArray))
//
//      nvh.underlyingActor.nodeView.state.persistentProver.digest
//
//      nvh ! ModifiersFromRemote(Header.modifierTypeId, Seq(HeaderProtoSerializer.toProto(newBlock.header).toByteArray))
//      nvh ! ModifiersFromRemote(Payload.modifierTypeId, Seq(PayloadProtoSerializer.toProto(newBlock.payload).toByteArray))
//
//      Thread.sleep(5000)
//    }
//  }
//}
