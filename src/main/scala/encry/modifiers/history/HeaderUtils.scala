package encry.modifiers.history

import com.typesafe.scalalogging.StrictLogging
import org.encryfoundation.common.modifiers.history.Header
import org.encryfoundation.common.validation.{ModifierValidator, ValidationResult}

object HeaderUtils extends StrictLogging {

  val TransactionsRootSize: Int = 32

  def syntacticallyValidity(header: Header, modifierIdSize: Int): ValidationResult = {
    logger.info(s"<----- START syntacticallyValidity START------->")
    logger.info(s"<----- $header ------->")
    logger.info(s"header.modifierTypeId == Header.modifierTypeId ${header.modifierTypeId == Header.modifierTypeId}")
    logger.info(s"header.id.size == modifierIdSize ${header.id.size == modifierIdSize}")
    logger.info(s"header.parentId.size == modifierIdSize ${header.parentId.size == modifierIdSize}" +
      s" ${header.parentId.size} -> $modifierIdSize")
    logger.info(s"header.transactionsRoot.size == TransactionsRootSize ${header.transactionsRoot.size == TransactionsRootSize}" +
      s" ${header.transactionsRoot.size} -> ${TransactionsRootSize}")
    val a = ModifierValidator.accumulateErrors
      .demand(header.modifierTypeId == Header.modifierTypeId,
        s"Modifier's type id should be ${Header.modifierTypeId}")
      .demand(header.id.size == modifierIdSize,
        s"Modifier's id should be $modifierIdSize bytes")
      .demand(header.parentId.size == modifierIdSize,
        s"Parent's id should be $modifierIdSize bytes")
      .demand(header.transactionsRoot.size == TransactionsRootSize,
        s"TransactionsRoot's size should be $TransactionsRootSize bytes")
      .result
    logger.info(s"<----- STOP syntacticallyValidity STOP------->")
    a
  }
}