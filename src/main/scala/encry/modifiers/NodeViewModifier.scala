package encry.modifiers

import com.typesafe.config.ConfigFactory
import encry.modifiers.mempool.Transaction
import encry.settings.Algos
import encry.{ModifierId, ModifierTypeId}
import org.encryfoundation.common.serialization.BytesSerializable
import org.encryfoundation.common.transaction.Proposition
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

  private val DefaultIdSize: Int = 32

  val ModifierIdSize: Int = Try(ConfigFactory.load().getConfig("app").getInt("modifierIdSize")).getOrElse(DefaultIdSize)
}

trait PersistentNodeViewModifier extends NodeViewModifier {
  def parentId: ModifierId
}

trait TransactionsCarryingPersistentNodeViewModifier[P <: Proposition, TX <: Transaction]
  extends PersistentNodeViewModifier {

  def transactions: Seq[TX]
}