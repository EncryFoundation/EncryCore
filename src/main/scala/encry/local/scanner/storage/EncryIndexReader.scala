package encry.local.scanner.storage

import encry.account.{Account, Address}
import encry.modifiers.state.box.EncryBox
import encry.modifiers.state.box.proposition.EncryProposition
import encry.storage.codec.FixLenComplexValueCodec
import encry.utils.ScorexLogging
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds.ADKey

case class EncryIndexReader(index: IndexStorage) extends ScorexLogging {

  def boxIdsByProposition(p: Proposition): Option[Seq[ADKey]] =
    index.get(IndexStorage.keyByProposition(p))
      .map(r => FixLenComplexValueCodec.parseComplexValue(r, EncryBox.BoxIdSize)
        .getOrElse(Seq.empty).map(ADKey @@ _))

  def boxIdsByAddress(address: Address): Option[Seq[ADKey]] =
    boxIdsByProposition(EncryProposition.accountLock(Account(address)))
}
