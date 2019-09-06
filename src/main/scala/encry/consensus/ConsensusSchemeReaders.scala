package encry.consensus

import encry.settings.Constants.constants

object ConsensusSchemeReaders {

  val consensusScheme: ConsensusScheme = {
    val schemeName = constants.ConsensusScheme
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
    val n = constants.n
    val k = constants.k
    EquihashPowScheme(n, k)
  }
}
