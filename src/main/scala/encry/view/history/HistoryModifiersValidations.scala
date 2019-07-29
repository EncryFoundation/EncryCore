package encry.view.history

import encry.view.history.HistoryValidationError.FatalValidationError._
import encry.view.history.HistoryValidationError.NonFatalValidationError._
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{Header, Payload}
import org.encryfoundation.common.utils.TaggedTypes.{Difficulty, ModifierId}
import org.encryfoundation.common.utils.constants.TestNetConstants
import org.encryfoundation.common.validation.ModifierSemanticValidity
import cats.syntax.either._
import encry.consensus.EquihashPowScheme
import encry.utils.NetworkTimeProvider

trait HistoryModifiersValidations extends HistoryExtension {

  val timeProvider = new NetworkTimeProvider(settings.ntp)
  val powScheme: EquihashPowScheme = EquihashPowScheme(TestNetConstants.n, TestNetConstants.k)

  def realDifficulty(h: Header): Difficulty = Difficulty !@@ powScheme.realDifficulty(h)


  def testApplicable(modifier: PersistentModifier): Either[ValidationError, PersistentModifier] = {
    val validationResult: Either[ValidationError, PersistentModifier] = modifier match {
      case header: Header => validate(header)
      case payload: Payload => validate(payload)
      case mod => UnknownModifierFatalError(s"Modifier $mod has incorrect type.").asLeft[PersistentModifier]
    }
    validationResult match {
      case Left(value) => logger.info(s"Validation result failed: $value"); validationResult
      case Right(m) => logger.info(s"Validation result successful for ${m.encodedId}"); validationResult
    }
  }

  def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity =
    history.get(validityKey(modifierId)) match {
      case Some(mod) if mod.headOption.contains(1.toByte) => ModifierSemanticValidity.Valid
      case Some(mod) if mod.headOption.contains(0.toByte) => ModifierSemanticValidity.Invalid
      case None if isModifierDefined(modifierId) => ModifierSemanticValidity.Unknown
      case None => ModifierSemanticValidity.Absent
      case mod => logger.error(s"Incorrect validity status: $mod")
        ModifierSemanticValidity.Absent
    }

  def validate(h: Header): Either[ValidationError, Header] =
    if (h.isGenesis) HeadersValidator.validateGenesisBlockHeader(h)
    else getHeaderById(h.parentId)
      .map(p => HeadersValidator.validateHeader(h, p))
      .getOrElse(HeaderNonFatalValidationError(s"Header's ${h.encodedId} parent doesn't contain in history").asLeft[Header])

  protected def validate(m: Payload): Either[ValidationError, PersistentModifier] =
    modifierValidation(m, getHeaderById(m.headerId))

  protected def modifierValidation(mod: PersistentModifier,
                                   headerOpt: Option[Header]): Either[ValidationError, PersistentModifier] = headerOpt
    .map(header => PayloadValidator.validate(mod, header, blockDownloadProcessor.minimalBlockHeight))
    .getOrElse(PayloadNonFatalValidationError(s"Header for ${mod.encodedId} doesn't contain in history").asLeft[PersistentModifier])

  object HeadersValidator {
    def validateGenesisBlockHeader(h: Header): Either[ValidationError, Header] = for {
      _ <- Either.cond(h.parentId.sameElements(Header.GenesisParentId), (),
        GenesisBlockFatalValidationError(s"Genesis block with header ${h.encodedId} should has genesis parent id"))
      _ <- Either.cond(getBestHeaderIdOpt.isEmpty, (),
        GenesisBlockFatalValidationError(s"Genesis block with header ${h.encodedId} appended to non-empty history"))
      _ <- Either.cond(h.height == TestNetConstants.GenesisHeight, (),
        GenesisBlockFatalValidationError(s"Height of genesis block with header ${h.encodedId} is incorrect"))
    } yield h

    def validateHeader(h: Header, parent: Header): Either[ValidationError, Header] = for {
      _ <- Either.cond(h.timestamp > parent.timestamp, (),
        HeaderFatalValidationError(s"Header ${h.encodedId} has timestamp ${h.timestamp} less than parent's ${parent.timestamp}"))
      _ <- Either.cond(h.height == parent.height + 1, (),
        HeaderFatalValidationError(s"Header ${h.encodedId} has height ${h.height} not greater by 1 than parent's ${parent.height}"))
      _ <- Either.cond(!history.containsMod(h.id), (),
        HeaderFatalValidationError(s"Header ${h.encodedId} is already in history"))
      _ <- Either.cond(realDifficulty(h) >= h.requiredDifficulty, (),
        HeaderFatalValidationError(s"Incorrect real difficulty in header ${h.encodedId}"))

      _ <- Either.cond(h.difficulty >= (requiredDifficultyAfter(parent) match {
        case Left(_) => 0
        case Right(value) => value
      }), (),
        HeaderFatalValidationError(s"Incorrect required difficulty in header ${h.encodedId}"))
      _ <- Either.cond(heightOf(h.parentId).exists(h => getBestHeaderHeight - h < TestNetConstants.MaxRollbackDepth), (),
        HeaderFatalValidationError(s"Header ${h.encodedId} has height greater than max roll back depth"))
      powSchemeValidationResult = powScheme.verify(h)
      _ <- Either.cond(powSchemeValidationResult.isRight, (),
        HeaderFatalValidationError(s"Wrong proof-of-work solution in header ${h.encodedId} caused: $powSchemeValidationResult"))
      _ <- Either.cond(isSemanticallyValid(h.parentId) != ModifierSemanticValidity.Invalid, (),
        HeaderFatalValidationError(s"Header ${h.encodedId} is semantically invalid"))
      _ <- Either.cond(h.timestamp - timeProvider.estimatedTime <= TestNetConstants.MaxTimeDrift, (),
        HeaderNonFatalValidationError(s"Header ${h.encodedId} with timestamp ${h.timestamp} is too far in future from now ${timeProvider.estimatedTime}"))
    } yield h
  }

  object PayloadValidator {

    def validate(m: PersistentModifier,
                 header: Header,
                 minimalHeight: Int): Either[ValidationError, PersistentModifier] = for {
      _ <- Either.cond(!history.containsMod(m.id), (),
        PayloadFatalValidationError(s"Modifier ${m.encodedId} is already in history"))
      _ <- Either.cond(header.isRelated(m), (),
        PayloadFatalValidationError(s"Modifier ${m.encodedId} does not corresponds to header ${header.encodedId}"))
      _ <- Either.cond(isSemanticallyValid(header.id) != ModifierSemanticValidity.Invalid, (),
        PayloadFatalValidationError(s"Header ${header.encodedId} for modifier ${m.encodedId} is semantically invalid"))
      _ <- Either.cond(header.height >= minimalHeight, (),
        PayloadNonFatalValidationError(s"Too old modifier ${m.encodedId}: ${header.height} < $minimalHeight"))
    } yield m
  }
}
