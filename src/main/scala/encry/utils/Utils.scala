package encry.utils

import com.google.common.primitives.Longs
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.mempool.transaction.EncryAddress.Address
import org.encryfoundation.common.modifiers.mempool.transaction.Pay2PubKeyAddress
import org.encryfoundation.common.network.BasicMessagesRepo.BasicMsgDataTypes.InvData
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ModifierId, ModifierTypeId}
import scorex.crypto.hash.Blake2b256
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey}
import scorex.utils.{Random => ScorexRandom}

object Utils {

  def nonceFromDigest(digest: Array[Byte]): Long = Longs.fromByteArray(Algos.hash(digest).take(8))

  def idsToString(ids: Seq[(ModifierTypeId, ModifierId)]): String = (ids.headOption, ids.lastOption) match {
    case (Some(f), Some(l)) if f._2 sameElements l._2 => s"[(${f._1},${Algos.encode(f._2)})]"
    case (Some(f), Some(l)) => s"[(${f._1},${Algos.encode(f._2)})..(${l._1},${Algos.encode(l._2)})]"
    case _ => "[]"
  }

  def idsToString(modifierType: ModifierTypeId, ids: Seq[ModifierId]): String =
    idsToString(ids.map(id => (modifierType, id)))

  def idsToString(invData: InvData): String = idsToString(invData._1, invData._2)

  def randomAddress: Address = Pay2PubKeyAddress(PublicKey @@ ScorexRandom.randomBytes()).address

  def protocolToBytes(protocol: String): Array[Byte] = protocol.split("\\.").map(elem => elem.toByte)

  def createPrivKey(seed: Option[String]): PrivateKey25519 = {
    val (privateKey: PrivateKey, publicKey: PublicKey) = Curve25519.createKeyPair(
      Blake2b256.hash(
        seed.map { Mnemonic.seedFromMnemonic(_) }
          .getOrElse {
            val phrase: String = Mnemonic.entropyToMnemonicCode(ScorexRandom.randomBytes(16))
            Mnemonic.seedFromMnemonic(phrase)
          })
    )
    PrivateKey25519(privateKey, publicKey)
  }

}