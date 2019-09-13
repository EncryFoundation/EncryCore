package encry.consensus

import encry.settings.Settings

object ConsensusSchemeReaders extends Settings {

  val consensusScheme: ConsensusScheme = {
    val schemeName = settings.constants.ConsensusScheme
    Seq(EquihashPowSchemeReader).find(_.schemeName == schemeName)
      .getOrElse(EquihashPowSchemeReader)
      .read
  }
}

sealed trait ConsensusSchemeReader[T <: ConsensusScheme] {
  def schemeName: String
  def read: T
}

object EquihashPowSchemeReader extends ConsensusSchemeReader[EquihashPowScheme] with Settings {

  val schemeName = "equihash"

  def read: EquihashPowScheme = {
    EquihashPowScheme(settings.constants.n, settings.constants.k, settings.constants.Version, settings.constants.PreGenesisHeight,
      settings.constants.MaxTarget)
  }
}
