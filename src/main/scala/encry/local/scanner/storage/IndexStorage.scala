package encry.local.scanner.storage

import encry.settings.Algos
import encry.storage.EncryBaseStorage
import io.iohk.iodb.{ByteArrayWrapper, Store}
import scorex.core.transaction.box.proposition.Proposition
import scorex.crypto.authds.ADKey

case class IndexStorage(store: Store) extends EncryBaseStorage

object IndexStorage {

  lazy val IndexVersionKey: ByteArrayWrapper = ByteArrayWrapper(Algos.hash("sync_state_version"))

  lazy val LastScannedBlockKey: ByteArrayWrapper = ByteArrayWrapper(Algos.hash("last_scanned_block"))

  def keyByBoxId(id: ADKey): ByteArrayWrapper = ByteArrayWrapper(id)

  def keyByProposition(p: Proposition): ByteArrayWrapper = ByteArrayWrapper(Algos.hash(p.bytes))
}
