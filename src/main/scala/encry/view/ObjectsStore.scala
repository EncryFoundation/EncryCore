package encry.view

import encry.modifiers.EncryPersistentModifier
import encry.ModifierId

import scala.util.Try

trait ObjectsStore {

  def get(id: ModifierId): Option[Array[Byte]]

  def delete(id: ModifierId): Try[Unit]

  def put(m: EncryPersistentModifier): Try[Unit]
}
