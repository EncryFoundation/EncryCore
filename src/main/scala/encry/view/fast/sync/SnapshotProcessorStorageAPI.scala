package encry.view.fast.sync

import SnapshotManifestProto.SnapshotManifestProtoMessage
import com.typesafe.scalalogging.StrictLogging
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.fast.sync.SnapshotHolder.{SnapshotManifest, SnapshotManifestSerializer}
import org.encryfoundation.common.utils.Algos

trait SnapshotProcessorStorageAPI extends StrictLogging {

  val storage: VersionalStorage

  def getManifestId(id: StorageKey): Option[Array[Byte]] = storage.get(id)

  def actualManifestId: Option[Array[Byte]] = getManifestId(ActualManifestKey)

  def manifestById(id: StorageKey): Option[SnapshotManifest] =
    storage
      .get(StorageKey @@ id)
      .flatMap(bytes => SnapshotManifestSerializer.fromProto(SnapshotManifestProtoMessage.parseFrom(bytes)).toOption)

  def actualManifest: Option[SnapshotManifest] = actualManifestId
    .flatMap(id => manifestById(StorageKey !@@ id))

  def potentialManifestsIds: Seq[Array[Byte]] =
    storage
      .get(PotentialManifestsIdsKey)
      .map(_.grouped(32).toSeq)
      .getOrElse(Seq.empty)

  def manifestBytesById(id: StorageKey): Option[StorageValue] = storage.get(id)

  val ActualManifestKey: StorageKey         = StorageKey @@ Algos.hash("actual_manifest_key")
  val PotentialManifestsIdsKey: StorageKey  = StorageKey @@ Algos.hash("potential_manifests_ids_key")
}
