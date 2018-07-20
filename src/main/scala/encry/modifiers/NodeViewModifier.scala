package encry.modifiers

import com.typesafe.config.ConfigFactory
import encry.modifiers.mempool.BaseTransaction
import encry.modifiers.serialization.BytesSerializable
import encry.settings.Algos
import encry.view.state.Proposition
import encry.{ModifierId, ModifierTypeId}
import scala.util.Try

trait NodeViewModifier extends BytesSerializable {

  val modifierTypeId: ModifierTypeId

  def id: ModifierId

  def encodedId: String = Algos.encode(id)

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: NodeViewModifier => (that.id sameElements id) && (that.modifierTypeId == modifierTypeId)
    case _ => false
  }
}

object NodeViewModifier {
  private val DefaultIdSize = 32

  val ModifierIdSize: Int = Try(ConfigFactory.load().getConfig("app").getInt("modifierIdSize")).getOrElse(DefaultIdSize)
}

trait PersistentNodeViewModifier extends NodeViewModifier {
  def parentId: ModifierId
}

trait TransactionsCarryingPersistentNodeViewModifier[P <: Proposition, TX <: BaseTransaction]
  extends PersistentNodeViewModifier {

  def transactions: Seq[TX]
}