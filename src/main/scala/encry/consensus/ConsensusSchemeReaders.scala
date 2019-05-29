package encry.consensus

import encry.settings.TestConstants

object ConsensusSchemeReaders {

  val consensusScheme: ConsensusScheme = {
    val schemeName = TestConstants.ConsensusScheme
    Seq(EquihashPowSchemeReader).find(_.schemeName == schemeName)
      .getOrElse(EquihashPowSchemeReader)
      .read
  }
}

sealed trait ConsensusSchemeReader[T <: ConsensusScheme] {
  def schemeName: String
  def read: T
}

object EquihashPowSchemeReader extends ConsensusSchemeReader[EquihashPowScheme] {

  val schemeName = "equihash"

  def read: EquihashPowScheme = {
    val n = TestConstants.n
    val k = TestConstants.k
    EquihashPowScheme(n, k)
  }
}
