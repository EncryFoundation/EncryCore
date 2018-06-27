package encry.modifiers

import com.typesafe.config.ConfigFactory
import encry.modifiers.mempool.Transaction
import encry.modifiers.serialization.BytesSerializable
import encry.view.state.Proposition
import encry.{ModifierId, ModifierTypeId}
import scorex.crypto.encode.Base58

import scala.util.Try

sealed trait NodeViewModifier extends BytesSerializable {
  self =>

  val modifierTypeId: ModifierTypeId

  def id: ModifierId

  def encodedId: String = Base58.encode(id)

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: NodeViewModifier => (that.id sameElements id) && (that.modifierTypeId == modifierTypeId)
    case _ => false
  }
}

trait EphemerealNodeViewModifier extends NodeViewModifier

object NodeViewModifier {
  private val DefaultIdSize = 32

  val ModifierIdSize: Int = Try(ConfigFactory.load().getConfig("app").getInt("modifierIdSize")).getOrElse(DefaultIdSize)
}

trait PersistentNodeViewModifier extends NodeViewModifier {
  def parentId: ModifierId
}

trait TransactionsCarryingPersistentNodeViewModifier[P <: Proposition, TX <: Transaction[P]]
  extends PersistentNodeViewModifier {

  def transactions: Seq[TX]
}