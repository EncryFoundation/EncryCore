package encry.view

import java.util.concurrent.atomic.AtomicReference

import org.encryfoundation.common.utils.TaggedTypes.ModifierId

object SerializedModsCache {

  val cacheRef: AtomicReference[Map[ModifierId, Array[Byte]]] = new AtomicReference(Map.empty)

  def putToCache(modId: ModifierId, value: Array[Byte]): Unit = cacheRef.getAndUpdate(map => map + (modId -> value))

  def get(modId: ModifierId): Option[Array[Byte]] = cacheRef.get().get(modId)
}
