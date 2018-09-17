package encry.local.explorer

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import encry.utils.CoreTaggedTypes.ModifierId
import encry.local.explorer.BlockListener.{ChainSwitching, NewOrphaned}
import encry.local.explorer.database.DBService
import encry.modifiers.history.block.EncryBlock
import encry.modifiers.history.block.header.EncryBlockHeader
import encry.modifiers.history.block.payload.EncryBlockPayload
import encry.modifiers.mempool.Transaction
import encry.network.EncryNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import encry.utils.EncryGenerator
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers.{eq => eq_}
import scala.concurrent.Future
import scala.concurrent.duration._

class BlockListenerSpec extends TestKit(ActorSystem("BlockListenerSpec")) with ImplicitSender
  with FlatSpecLike with Matchers with BeforeAndAfterAll with MockitoSugar with EncryGenerator {

  "BlockListener" should "process valid blocks" in new BlockListenerSpecWiring {
    when(dbServiceMock.processBlock(sampleBlock)).thenReturn(Future.successful(100))

    actor ! sampleModifier
    expectNoMsg(1 second)
    verify(dbServiceMock).selectHeightOpt
    verify(dbServiceMock).processBlock(eq_(sampleBlock))
  }

  it should "process valid chain switching msg" in new BlockListenerSpecWiring {
    when(dbServiceMock.markAsRemovedFromMainChain(sampleSwitchedIds)).thenReturn(Future.successful(100))

    actor ! sampleChainSwitching
    expectNoMsg(1 second)
    verify(dbServiceMock).markAsRemovedFromMainChain(eq_(sampleSwitchedIds))
  }

  it should "process new orphans" in new BlockListenerSpecWiring {
    when(dbServiceMock.processOrphanedHeader(sampleHeader)).thenReturn(Future.successful(1))

    actor ! sampleNewOrphaned
    expectNoMsg(1 second)
    verify(dbServiceMock).processOrphanedHeader(eq_(sampleHeader))
  }

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  // internal

  private trait BlockListenerSpecWiring {
    val noHeight: Future[Option[Int]] = Future.successful(None)
    val dbServiceMock: DBService = mock[DBService]
    when(dbServiceMock.selectHeightOpt).thenReturn(Future.successful(None))
    val actor: ActorRef = system.actorOf(Props(new BlockListener(dbServiceMock)))
    val sampleHeader: EncryBlockHeader = genHeader
    val sampleTxs: Seq[Transaction] = genValidPaymentTxs(100)
    val samplePayload: EncryBlockPayload = EncryBlockPayload(sampleHeader.id, sampleTxs)
    val sampleBlock: EncryBlock = EncryBlock(sampleHeader, samplePayload, None)
    val sampleModifier: SemanticallySuccessfulModifier[EncryBlock] = SemanticallySuccessfulModifier(sampleBlock)
    val sampleNewOrphaned: NewOrphaned = NewOrphaned(sampleHeader)
    val sampleSwitchedIds: List[ModifierId] = sampleTxs.map(_.id).toList
    val sampleChainSwitching: ChainSwitching = ChainSwitching(sampleSwitchedIds)
  }

}
