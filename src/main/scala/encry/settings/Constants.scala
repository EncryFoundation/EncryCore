package encry.settings

import encry.consensus.{Difficulty, DifficultySerializer}
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

    val hashLength: Int = 32

    val MaxTarget: BigInt = BigInt(1, Array.fill(hashLength)((-1).toByte))

    val InitialDifficulty: Difficulty = Difficulty @@ BigInt(1)

    val InitialNBits: Long = DifficultySerializer.encodeCompactBits(InitialDifficulty)

    val ModifierIdSize: Int = hashLength

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

    // Number of last epochs that will  be used for difficulty recalculation
    val RetargetingEpochsQty = 30

    val EpochLength = 1

    val BlockMaxSize = 10000

    val GenesisHeight: Height = Height @@ 0

    val PreGenesisHeight: Height = Height @@ (GenesisHeight - 1)

    val MaxRollback: Long = 600.days.toMillis / DesiredBlockInterval.toMillis

    val MaxTimeDrift: Long = 10 * DesiredBlockInterval.toMillis

    val TokenSymbolMaxLength: Int = 10
  }
}
