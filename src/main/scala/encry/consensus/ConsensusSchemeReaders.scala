package encry.consensus

import encry.settings.Constants

object ConsensusSchemeReaders {

  val readers: Seq[ConsensusSchemeReader[_ <: ConsensusScheme]] = Seq(
    EquihashPowSchemeReader
  )

  val consensusScheme: ConsensusScheme = {
    val schemeName = Constants.Chain.ConsensusScheme
    readers.find(_.schemeName == schemeName)
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
    val n = Constants.Equihash.n
    val k = Constants.Equihash.k
    EquihashPowScheme(n, k)
  }
}
