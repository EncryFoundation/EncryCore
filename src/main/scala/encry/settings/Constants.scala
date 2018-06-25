package encry.settings

import encry.consensus.{Difficulty, DifficultySerializer, NBits}
import encry.view.history.Height
import encry.modifiers.history.block.Block.Version
import scorex.crypto.authds.ADKey

import scala.concurrent.duration._

object Constants {

  val DigestLength: Int = 32

  val ModifierIdSize: Int = DigestLength

  // Maximum block size in bytes
  val BlockMaxSize: Int = 1000000

  // Maximum transaction size in bytes
  val TransactionMaxSize: Int = BlockMaxSize / 4

  val DefaultKeepVersions: Int = 200  // TODO: Move to `NodeSettings`.

  val PersistentByteCost: Int = 16

  val StateByteCost: Int = 26

  // Maximum number of computational `steps` contract can use.
  val ContractMaxFuel: Int = 800

  val MaxDataLength: Int = 1000

  val AfterGenesisStateDigestHex: String = "2095f25398b6430c52cf8d91f76936896282d78f2fafd38d4d9529323742381810"

  val IntrinsicTokenId: ADKey = ADKey !@@ Algos.hash("intrinsic_token")

  object Chain {

    val ConsensusScheme: String = "equihash"

    val HashLength: Int = 32

    val MaxTarget: BigInt = BigInt(1, Array.fill(HashLength)((-1).toByte))

    val InitialDifficulty: Difficulty = Difficulty @@ BigInt(1)

    val Version: Version = 0: Byte

    val InitialNBits: NBits = DifficultySerializer.encodeCompactBits(InitialDifficulty)

    val ModifierIdSize: Int = HashLength

    val CoinbaseHeightLock: Int = 1

    val GenesisBoxesAmount: Long = 1L

    // Number of fractions in one Encry Token.
    val FractionsInOneCoin: Int = 1000000

    val InitialEmissionAmount: Int = 2 * FractionsInOneCoin

    val DeflationInterval: Int = 100

    val DeflationFactor: Double = 0.9998

    val EmissionEpochLength: Int = 10000

    // Desired time interval between blocks
    val DesiredBlockInterval: FiniteDuration = 30.seconds

    // Number of last epochs for difficulty recalculation
    val RetargetingEpochsQty: Int = 3

    val EpochLength: Int = 10

    val GenesisHeight: Height = Height @@ 0

    val PreGenesisHeight: Height = Height @@ (GenesisHeight - 1)

    // Maximum number of epochs blockchain state can be rolled back
    val MaxRollbackDepth: Int = (10.days.toMillis / DesiredBlockInterval.toMillis).toInt

    val MaxTimeDrift: Long = 10 * DesiredBlockInterval.toMillis

    val TokenSymbolMaxLength: Int = 10
  }

  object Equihash {

    val n: Char = 96

    val k: Char = 5
  }

}
