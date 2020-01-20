package encry.view.history

import com.google.common.primitives.Ints
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.storage.VersionalStorage.StorageKey
import encry.view.history.storage.HistoryStorage
import org.encryfoundation.common.modifiers.history.{Block, Header, Payload}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{Height, ModifierId, ModifierTypeId}
import scorex.crypto.hash.Digest32

import scala.reflect.ClassTag

trait HistoryDBApi extends StrictLogging {

  val settings: EncryAppSettings

  val historyStorage: HistoryStorage

}