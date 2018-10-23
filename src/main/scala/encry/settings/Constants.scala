package encry.settings

import encry.consensus.ConsensusTaggedTypes.Difficulty
import encry.modifiers.history.Block.Version
import encry.view.history.History.Height
import org.encryfoundation.common.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import scala.concurrent.duration._

object Constants {

  val DigestLength: Int = 32

  val ModifierIdSize: Int = DigestLength

  // Maximum block payload size in bytes
  val PayloadMaxSize: Int = 1000000

  // Maximum block header size in bytes
  val HeaderMaxSize: Int = 200

  val DefaultKeepVersions: Int = 200

  val PersistentByteCost: Int = 16

  val StateByteCost: Int = 26

  val MaxDataLength: Int = 1000

  val AfterGenesisStateDigestHex: String = "39bc0df81fec1ee7b3804eb42084883ce4925aec8393df39806f00e129ade9f101"

  val GenesisStateVersion: String = "0909090909090909090909090909090909090909090909090909090909090909"

  val IntrinsicTokenId: ADKey = ADKey !@@ Algos.hash("intrinsic_token")

  object Chain {

    val ConsensusScheme: String = "equihash"

    val HashLength: Int = 32

    val MaxTarget: BigInt = BigInt(1, Array.fill(HashLength)((-1).toByte))

    val InitialDifficulty: Difficulty = Difficulty @@ BigInt(1)

    val Version: Version = 0: Byte

    val InitialEmissionAmount: Int = 1000000000

    val EmissionDecay = 0.05

    val EmissionEpochLength: Int = 5040

    // Desired time interval between blocks
    val DesiredBlockInterval: FiniteDuration = 120.seconds

    val NewHeaderTimeMultiplier: Int = 5

    // Number of last epochs for difficulty recalculation
    val RetargetingEpochsQty: Int = 3

    val EpochLength: Int = 10

    val GenesisHeight: Height = Height @@ 0

    val PreGenesisHeight: Height = Height @@ (GenesisHeight - 1)

    // Maximum number of epochs blockchain state can be rolled back
    val MaxRollbackDepth: Int = 100

    // Maximum delta any timestamp can differ from current estimated time
    val MaxTimeDrift: Long = (2 hours).toMillis
  }

  object Equihash {

    val n: Char = 96

    val k: Char = 5
  }

}
