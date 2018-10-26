package encry.view

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}

class EncryNodeViewHolderTest extends TestKit(ActorSystem("SenderTestSystem")) with ImplicitSender
  with FlatSpecLike with Matchers with BeforeAndAfterAll {

}
