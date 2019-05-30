package encry.consensus

import org.encryfoundation.common.utils.constants.TestNetConstants

object ConsensusSchemeReaders {

  val consensusScheme: ConsensusScheme = {
    val schemeName = TestNetConstants.ConsensusScheme
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
    val n = TestNetConstants.n
    val k = TestNetConstants.k
    EquihashPowScheme(n, k)
  }
}
