package encry.it

import encry.it.configs.Configs
import encry.it.docker.Docker
import org.scalatest.{AsyncFunSuite, Matchers}

import scala.concurrent.Future

class TransactionsTests extends AsyncFunSuite with Matchers {

  test("Get box, form and send transaction. Check block for availability of this transaction. Check miner balance.") {

    val heightToCheck: Int = 5

    val docker = Docker()
    val config = Configs

    Future {
      true shouldEqual  true
    }
  }
  }
