package encry.view.history

import cats.syntax.either._
import encry.consensus.EquihashPowScheme
import encry.view.history.ValidationError.FatalValidationError._
import encry.view.history.ValidationError.NonFatalValidationError._
import org.encryfoundation.common.modifiers.PersistentModifier
import org.encryfoundation.common.modifiers.history.{ Header, Payload }
import org.encryfoundation.common.utils.TaggedTypes.{ Difficulty, ModifierId }
import org.encryfoundation.common.validation.ModifierSemanticValidity

trait HistoryModifiersValidator extends HistoryApi {

  lazy val powScheme: EquihashPowScheme = EquihashPowScheme(settings.constants.n,
                                                            settings.constants.k,
                                                            settings.constants.Version,
                                                            settings.constants.PreGenesisHeight,
                                                            settings.constants.MaxTarget)

  def testApplicable(modifier: PersistentModifier): Either[ValidationError, PersistentModifier] =
    (modifier match {
      case header: Header   => validateHeader(header)
      case payload: Payload => validatePayload(payload)
      case mod              => UnknownModifierFatalError(s"Modifier $mod has incorrect type.").asLeft[PersistentModifier]
    }) match {
      case l @ Left(value) => logger.info(s"Validation result for ${modifier.encodedId} failed cause $value"); l
      case r @ Right(_)    => r
    }

  private def validateHeader(h: Header): Either[ValidationError, Header] =
    if (h.isGenesis) genesisBlockHeaderValidator(h)
    else
      getHeaderById(h.parentId)
        .map(p => headerValidator(h, p))
        .getOrElse(
          HeaderNonFatalValidationError(s"Header's ${h.encodedId} parent doesn't contain in history").asLeft[Header]
        )

  private def validatePayload(mod: Payload): Either[ValidationError, PersistentModifier] =
    getHeaderById(mod.headerId)
      .map(header => payloadValidator(mod, header, blockDownloadProcessor.minimalBlockHeight))
      .getOrElse(
        PayloadNonFatalValidationError(s"Header for ${mod.encodedId} doesn't contain in history")
          .asLeft[PersistentModifier]
      )

  private def realDifficulty(h: Header): Difficulty = Difficulty !@@ powScheme.realDifficulty(h)

  private def isSemanticallyValid(modifierId: ModifierId): ModifierSemanticValidity =
    historyStorage
      .get(validityKey(modifierId)) match {
      case Some(mod) if mod.headOption.contains(1.toByte) => ModifierSemanticValidity.Valid
      case Some(mod) if mod.headOption.contains(0.toByte) => ModifierSemanticValidity.Invalid
      case None if isModifierDefined(modifierId)          => ModifierSemanticValidity.Unknown
      case None                                           => ModifierSemanticValidity.Absent
      case mod =>
        logger.error(s"Incorrect validity status: $mod")
        ModifierSemanticValidity.Absent
    }

  private def genesisBlockHeaderValidator(h: Header): Either[ValidationError, Header] =
    for {
      _ <- Either.cond(
            h.parentId.sameElements(Header.GenesisParentId),
            (),
            GenesisBlockFatalValidationError(s"Genesis block with header ${h.encodedId} should has genesis parent id")
          )
      _ <- Either.cond(
            getBestHeaderId.isEmpty,
            (),
            GenesisBlockFatalValidationError(s"Genesis block with header ${h.encodedId} appended to non-empty history")
          )
      _ <- Either.cond(
            h.height == settings.constants.GenesisHeight,
            (),
            GenesisBlockFatalValidationError(s"Height of genesis block with header ${h.encodedId} is incorrect")
          )
    } yield h

  private def headerValidator(h: Header, parent: Header): Either[ValidationError, Header] =
    for {
      _ <- Either.cond(
            h.timestamp > parent.timestamp,
            (),
            HeaderFatalValidationError(
              s"Header ${h.encodedId} has timestamp ${h.timestamp}" +
                s" less than parent's ${parent.timestamp}"
            )
          )
      _ <- Either.cond(
            h.height == parent.height + 1,
            (),
            HeaderFatalValidationError(
              s"Header ${h.encodedId} has height ${h.height}" +
                s" not greater by 1 than parent's ${parent.height}"
            )
          )
      _ <- Either.cond(!historyStorage.containsMod(h.id),
                       (),
                       HeaderFatalValidationError(s"Header ${h.encodedId} is already in history"))
      _ <- Either.cond(realDifficulty(h) >= h.requiredDifficulty,
                       (),
                       HeaderFatalValidationError(s"Incorrect real difficulty in header ${h.encodedId}"))
      _ <- Either.cond(requiredDifficultyAfter(parent).exists(_ <= h.difficulty),
                       (),
                       HeaderFatalValidationError(s"Incorrect required difficulty in header ${h.encodedId}"))
      _ <- Either.cond(
            heightOf(h.parentId).exists(h => getBestHeaderHeight - h < settings.constants.MaxRollbackDepth),
            (),
            HeaderFatalValidationError(s"Header ${h.encodedId} has height greater than max roll back depth")
          )
      powSchemeValidationResult = powScheme.verify(h)
      _ <- Either.cond(
            powSchemeValidationResult.isRight,
            (),
            HeaderFatalValidationError(
              s"Wrong proof-of-work solution in header ${h.encodedId}" +
                s" caused: $powSchemeValidationResult"
            )
          )
      _ <- Either.cond(isSemanticallyValid(h.parentId) != ModifierSemanticValidity.Invalid,
                       (),
                       HeaderFatalValidationError(s"Header ${h.encodedId} is semantically invalid"))
      _ <- Either.cond(
            h.timestamp - timeProvider.estimatedTime <= settings.constants.MaxTimeDrift,
            (),
            HeaderNonFatalValidationError(
              s"Header ${h.encodedId} with timestamp ${h.timestamp}" +
                s" is too far in future from now ${timeProvider.estimatedTime}"
            )
          )
    } yield h

  private def payloadValidator(m: PersistentModifier,
                               header: Header,
                               minimalHeight: Int): Either[ValidationError, PersistentModifier] =
    for {
      _ <- Either.cond(!historyStorage.containsMod(m.id),
                       (),
                       PayloadFatalValidationError(s"Modifier ${m.encodedId} is already in history"))
      _ <- Either.cond(
            header.isRelated(m),
            (),
            PayloadFatalValidationError(s"Modifier ${m.encodedId} does not corresponds to header ${header.encodedId}")
          )
      _ <- Either.cond(
            isSemanticallyValid(header.id) != ModifierSemanticValidity.Invalid,
            (),
            PayloadFatalValidationError(
              s"Header ${header.encodedId} for modifier ${m.encodedId} is semantically invalid"
            )
          )
      _ <- Either.cond(
            header.height >= minimalHeight,
            (),
            PayloadNonFatalValidationError(s"Too old modifier ${m.encodedId}: ${header.height} < $minimalHeight")
          )
    } yield m
}
