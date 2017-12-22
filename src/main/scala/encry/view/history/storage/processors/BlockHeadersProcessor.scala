package encry.view.history.storage.processors

import com.google.common.primitives.Ints
import encry.consensus.{Difficulty, PowLinearController}
import encry.settings.Constants._
import encry.modifiers.EncryPersistentModifier
import encry.modifiers.history.block.header.{EncryBlockHeader, EncryHeaderChain}
import encry.settings.{Algos, ConsensusSettings}
import encry.view.history.Height
import encry.view.history.storage.HistoryStorage
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.ModifierId
import scorex.core.consensus.History

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

trait BlockHeadersProcessor {

  protected val consensusSettings: ConsensusSettings

  protected lazy val powLinearController = new PowLinearController(consensusSettings)

  protected val BestHeaderKey: ByteArrayWrapper =
    ByteArrayWrapper(Array.fill(hashLength)(EncryBlockHeader.modifierTypeId))

  protected val BestFullBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(hashLength)(-1))

  protected val historyStorage: HistoryStorage

  def typedModifierById[T <: EncryPersistentModifier](id: ModifierId): Option[T]

  protected def bestHeaderIdOpt: Option[ModifierId] = historyStorage.db.get(BestHeaderKey).map(ModifierId @@ _.data)

  def headersHeight: Int = bestHeaderIdOpt.flatMap(id => heightOf(id)).getOrElse(-1)

  protected def process(header: EncryBlockHeader): History.ProgressInfo[EncryPersistentModifier]

  protected def validate(header: EncryBlockHeader): Try[Unit] = {
    lazy val parentOpt = typedModifierById[EncryBlockHeader](header.parentId)
    if (header.parentId sameElements EncryBlockHeader.GenesisParentId)
      if (header.height != ConsensusSettings.genesisHeight)
        Failure(new Error("Invalid height for genesis block header."))
      Success()
    if (parentOpt.isEmpty)
      Failure(new Error(s"Parental header <id: ${header.parentId}> does not exist!"))
    if (header.height != parentOpt.get.height + 1)
      Failure(new Error(s"Invalid height in header <id: ${header.id}>"))
    if (!header.validTimestamp)
      Failure(new Error(s"Invalid timestamp in header <id: ${header.id}>"))
    if (header.timestamp < parentOpt.get.timestamp)
      Failure(new Error("Header timestamp is less than parental`s"))
    if (requiredDifficultyAfter(parentOpt.get) > header.difficulty)
      Failure(new Error("Header <id: ${header.id}> difficulty too low."))
    if (!header.validPow)
      Failure(new Error(s"Invalid POW in header <id: ${header.id}>"))
    Success()
  }

  protected def headerHeightKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Algos.hash("height".getBytes ++ id))

  def heightOf(id: ModifierId): Option[Int] =
    historyStorage.db
      .get(headerHeightKey(id))
      .map(b => Ints.fromByteArray(b.data))

  def isInBestChain(id: ModifierId): Boolean = heightOf(id).flatMap(h => bestHeaderIdAtHeight(h))
    .exists(_ sameElements id)

  private def bestHeaderIdAtHeight(h: Int): Option[ModifierId] = headerIdsAtHeight(h).headOption

  private def heightIdsKey(height: Int): ByteArrayWrapper = ByteArrayWrapper(Algos.hash(Ints.toByteArray(height)))

  /**
    * @param height - block height
    * @return ids of headers on chosen height.
    *         Seq.empty we don't have any headers on this height (e.g. it is too big or we bootstrap in PoPoW regime)
    *         single id if no forks on this height
    *         multiple ids if there are forks at chosen height.
    *         First id is always from the best headers chain.
    */
  def headerIdsAtHeight(height: Int): Seq[ModifierId] =
    ModifierId @@ historyStorage.db.get(heightIdsKey(height: Int)).map(_.data).getOrElse(Array()).grouped(32).toSeq

  /**
    * @param limit       - maximum length of resulting HeaderChain
    * @param startHeader - header to start
    * @param until       - stop condition
    * @return at most limit header back in history starting from startHeader and when condition until is not satisfied
    *         Note now it includes one header satisfying until condition!
    */
  protected def headerChainBack(limit: Int, startHeader: EncryBlockHeader,
                                until: EncryBlockHeader => Boolean): EncryHeaderChain = {
    @tailrec
    def loop(header: EncryBlockHeader, acc: Seq[EncryBlockHeader]): Seq[EncryBlockHeader] = {
      if (acc.length == limit || until(header)) {
        acc
      } else {
        typedModifierById[EncryBlockHeader](header.parentId) match {
          case Some(parent: EncryBlockHeader) =>
            loop(parent, acc :+ parent)
          case None if acc.contains(header) =>
            acc
          case _ =>
            acc :+ header
        }
      }
    }

    if (bestHeaderIdOpt.isEmpty || (limit == 0)) EncryHeaderChain(Seq())
    else EncryHeaderChain(loop(startHeader, Seq(startHeader)).reverse)
  }

  def requiredDifficultyAfter(parent: EncryBlockHeader): Difficulty = {
    val parentHeight = heightOf(parent.id).get
    val requiredHeadersHeights = powLinearController.epochsHeightsForRetargetingAt(Height @@ (parentHeight + 1))
    assert(requiredHeadersHeights.last == parentHeight, "Incorrect heights sequence!")
    val chain = headerChainBack(requiredHeadersHeights.max - requiredHeadersHeights.min + 1,
      parent, (_: EncryBlockHeader) => false)
    powLinearController.getNewDifficulty(parent.difficulty, powLinearController.getLastEpochsInterval(chain.headers))
  }
}
