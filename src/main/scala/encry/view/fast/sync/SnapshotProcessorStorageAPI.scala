package encry.view.fast.sync

import SnapshotManifestProto.SnapshotManifestProtoMessage
import com.typesafe.scalalogging.StrictLogging
import encry.storage.VersionalStorage
import encry.storage.VersionalStorage.{StorageKey, StorageValue}
import encry.view.fast.sync.SnapshotHolder.{SnapshotManifest, SnapshotManifestSerializer}
import org.encryfoundation.common.utils.Algos

trait SnapshotProcessorStorageAPI extends StrictLogging {

  val storage: VersionalStorage

  def actualManifest: Option[SnapshotManifest] = manifestById(ActualManifestKey)

  def bestPotentialManifest: Option[SnapshotManifest] = manifestById(BestPotentialManifestKey)

  def potentialManifestsIds: Seq[Array[Byte]] =
    storage
      .get(PotentialManifestsIdsKey)
      .map(_.grouped(32).toSeq)
      .getOrElse(Seq.empty)

  def manifestById(id: StorageKey): Option[SnapshotManifest] =
    storage
      .get(StorageKey @@ id)
      .flatMap(bytes => SnapshotManifestSerializer.fromProto(SnapshotManifestProtoMessage.parseFrom(bytes)).toOption)

  def manifestBytesById(id: StorageKey): Option[StorageValue] = storage.get(id)

  def allAvailableChunkKeys: Seq[Array[Byte]] =
    storage.get(CurrentAvailableChunksKey).map(_.grouped(32).toSeq).getOrElse(Seq.empty)

  def allPotentialManifests: Seq[SnapshotManifest] = potentialManifestsIds.flatMap(m => manifestById(StorageKey @@ m))

  val ActualManifestKey: StorageKey         = StorageKey @@ Algos.hash("actual_manifest_key")
  val BestPotentialManifestKey: StorageKey  = StorageKey @@ Algos.hash("best_potential_manifest_key")
  val PotentialManifestsIdsKey: StorageKey  = StorageKey @@ Algos.hash("potential_manifests_ids_key")
  val CurrentAvailableChunksKey: StorageKey = StorageKey @@ Algos.hash("current_available_chunks_key")
}
