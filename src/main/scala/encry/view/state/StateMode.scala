package encry.view.state

import io.circe.Encoder

sealed trait StateMode {

  def verboseName: String

  def isDigest: Boolean = this == StateMode.Digest

  def isUtxo: Boolean = !isDigest

  override def toString: String = verboseName
}

object StateMode {

  case object Utxo extends StateMode {

    def verboseName: String = "utxo"
  }

  case object Digest extends StateMode {

    def verboseName: String = "digest"
  }

  type UtxoMode = Utxo.type
  type DigestMode = Digest.type

  val values: Seq[StateMode] = Seq(Utxo, Digest)

  /** This class allows to check the correspondence between concrete instances of [[StateMode]] and [[EncryState]] */
  sealed trait Evidence[ST <: StateMode, S <: EncryState[S]]

  implicit object UtxoEvidence extends Evidence[UtxoMode, UtxoState]

  implicit object DigestEvidence extends Evidence[DigestMode, DigestState]

  implicit val stateModeEncoder: Encoder[StateMode] =
    Encoder.encodeString.contramap[StateMode](_.verboseName)
}
