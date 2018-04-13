package encry.local.scanner.storage

import encry.account.Address
import encry.modifiers.state.box.EncryBox
import encry.modifiers.state.box.proposition.AccountProposition
import encry.settings.Algos
import encry.storage.codec.FixLenComplexValueCodec
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADKey

class EncryIndexReader(val index: IndexStorage) extends ScorexLogging {

  def boxIdsByProposition(p: Proposition): Option[Seq[ADKey]] = {

    println(s"Trying to get to ${Algos.encode(p.bytes)}")
    index.get(IndexStorage.keyByProposition(p)).foreach(key => FixLenComplexValueCodec.parseComplexValue(key, EncryBox.BoxIdSize).get.foreach(key1 => println(" ---IN API KEY: " + Algos.encode(key1))))

    index.get(IndexStorage.keyByProposition(p))
      .map(r => FixLenComplexValueCodec.parseComplexValue(r, EncryBox.BoxIdSize)
        .getOrElse(Seq.empty).map(ADKey @@ _))
  }

  def boxIdsByAddress(address: Address): Option[Seq[ADKey]] =
    boxIdsByProposition(AccountProposition(address))
}
