package encry.network.DeliveryManagerTests

import akka.actor.{ Actor, ActorRef, ActorSystem, Props }
import encry.modifiers.InstanceFactory
import encry.settings.TestNetSettings
import org.scalatest.{ BeforeAndAfterAll, Matchers, OneInstancePerTest, WordSpecLike }

class wert
    extends WordSpecLike
    with BeforeAndAfterAll
    with Matchers
    with InstanceFactory
    with OneInstancePerTest
    with TestNetSettings {

  implicit val system: ActorSystem = ActorSystem("SynchronousTestingSpec")

  trait ABC {
    def update() = qwerty += 1
    def show     = println("qwerty " + qwerty)
    var qwerty   = 1
  }

  class A extends Actor {

    val f = new ABC {}

    override def receive: Receive = {
      case "ui"   =>
        println("ui")
        f.update()
      case YUI(r) =>
        println("YUI")
        r ! f
    }
  }

  case class YUI(ref: ActorRef)

  class B extends Actor {

    override def preStart(): Unit = println("qui")

    var m: Option[ABC] = None

    override def receive: Receive = {
      case a: ABC =>
        println(s"ABC _> ABC")
        m = Some(a)
      case "get" =>
        println("get")
        m.map(_.show)
    }

  }

  "test" should {
    "ghj" in {
      val actorA = system.actorOf(Props(new A), "A")
      val actorB = system.actorOf(Props(new B), "B")

      Thread.sleep(1000)

      actorB ! "get"
      actorA ! YUI(actorB)
      Thread.sleep(1000)
      actorB ! "get"
      actorA ! "ui"
      actorB ! "get"
//      actorA ! YUI(actorB)
//      Thread.sleep(1000)
//      actorB ! "get"
    }
  }



  trait QWE {
    var k = 0
    def updateK = k += 1
    var a: Int
  }

  class QWEIM extends QWE {
    var a: Int = 1
    def update = a += 1
    def show = println("a " + a)
  }

  "t" should {
    "hgjfkdls" in {
      val ag = new QWEIM
      ag.updateK
    }
  }
}
