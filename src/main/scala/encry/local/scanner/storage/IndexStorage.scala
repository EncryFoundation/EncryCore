package encry.local.scanner.storage

import encry.settings.Algos
import encry.storage.EncryBaseStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds.ADKey

class IndexStorage(val store: Store) extends EncryBaseStorage

object IndexStorage {

  val IndexVersionKey: ByteArrayWrapper = ByteArrayWrapper(Algos.hash("sync_state_version"))

  def keyByBoxId(id: ADKey): ByteArrayWrapper = ByteArrayWrapper(id)

  def keyByProposition(p: Proposition): ByteArrayWrapper = ByteArrayWrapper(Algos.hash(p.bytes))
}
