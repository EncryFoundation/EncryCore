package encry.consensus

import encry.settings.ConstantsSettings

object ConsensusSchemeReaders extends ConstantsSettings {

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

object EquihashPowSchemeReader extends ConsensusSchemeReader[EquihashPowScheme] with ConstantsSettings {

  val schemeName = "equihash"

  def read: EquihashPowScheme = {
    EquihashPowScheme(constants.n, constants.k, constants.Version, constants.PreGenesisHeight, constants.MaxTarget)
  }
}
