package encry.settings

import encry.consensus.Difficulty
import encry.view.history.Height

import scala.concurrent.duration._

object Constants {

  val DigestLength: Int = 32

  val ModifierIdSize: Int = DigestLength

  val TransactionMaxSize: Int = 400

  val DefaultKeepVersions: Int = 200  // TODO: Move to `NodeSettings`.

  val FeeMinAmount: Int = 100

  val PersistentByteCost: Int = 16

  val StateByteCost: Int = 26

  // Maximum number of computational `steps` contract can use.
  val ContractMaxFuel: Int = 800

  object Chain {

    val CoinbaseHeightLock = 1

    val GenesisBoxesQty = 100

    val GenesisBoxesAmount = 1L

    // Number of fractions in one Encry Token.
    val FractionsInOneCoin = 1000000

    val InitialEmissionAmount: Int = 2 * FractionsInOneCoin

    val DeflationInterval = 100

    val DeflationFactor = 0.9998

    // Desired time interval between blocks
    val DesiredBlockInterval: FiniteDuration = 30.seconds

    val InitialDifficulty: Difficulty = Difficulty @@ BigInt(100000)

    // Number of last epochs that will  be used for difficulty recalculation
    val RetargetingEpochsQty = 30

    val EpochLength = 1

    val BlockMaxSize = 10000

    val MaxTarget = BigInt(1, Array.fill(DigestLength)((-1).toByte))

    val GenesisHeight: Height = Height @@ 1

    val PreGenesisHeight: Height = Height @@ (GenesisHeight - 1)

    val MaxRollback: Long = 600.days.toMillis / DesiredBlockInterval.toMillis

    val MaxTimeDrift: Long = 10 * DesiredBlockInterval.toMillis
  }
}
