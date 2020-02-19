package encry.view.wallet

import cats.implicits._
import com.google.common.primitives.Longs
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{
  LevelDBVersion,
  VersionalLevelDbKey,
  VersionalLevelDbValue
}
import encry.storage.levelDb.versionalLevelDB.{ LevelDbDiff, VersionalLevelDB, VersionalLevelDBCompanion }
import encry.utils.BalanceCalculator
import org.encryfoundation.common.modifiers.state.StateModifierSerializer
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.modifiers.state.box.{ AssetBox, DataBox, EncryBaseBox, TokenIssuingBox }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ ADKey, ModifierId }
import org.encryfoundation.prismlang.compiler.CompiledContract.ContractHash
import supertagged.@@

import scala.annotation.tailrec

class WalletDBImpl private (
  levelDb: VersionalLevelDB,
  settings: EncryAppSettings
) extends WalletDB
    with StrictLogging
    with AutoCloseable {

  override def getAllWallets: List[ContractHash] =
    levelDb
      .get(CONTRACT_HASH_ACCOUNTS)
      .map(_.grouped(32).toList)
      .getOrElse(List.empty[ContractHash])

  override def getBoxById(id: ADKey): Option[EncryBaseBox] =
    levelDb
      .get(VersionalLevelDbKey @@ id.untag(ADKey))
      .flatMap(StateModifierSerializer.parseBytes(_, id.head).toOption)

  def getBalances: Map[ContractHash, Map[TokenId, Amount]] =
    getAllWallets.map(hash => hash -> getBalancesByContractHash(hash)).toMap

  def getTokenIds(hash: ContractHash): List[TokenId] =
    levelDb
      .get(hashToTokens(hash))
      .map(_.grouped(32).toList)
      .getOrElse(List.empty[TokenId])

  private def getBoxesIdsByKey(key: VersionalLevelDbKey): List[ADKey] =
    levelDb
      .get(key)
      .map(_.grouped(32).toList.map(ADKey @@ _))
      .getOrElse(List.empty[ADKey])

  @tailrec
  private def loop[BXT](acc: List[BXT], ids: List[ADKey], f: List[BXT] => Boolean): List[BXT] =
    ids.headOption match {
      case Some(boxId: ADKey) =>
        val newAcc: List[BXT] = getBoxById(boxId).fold(acc)(_ :: acc)
        if (f(newAcc)) newAcc else loop(newAcc, ids.drop(1), f)
      case None => List.empty[BXT]
    }

  //List.empty
  override def getAssetBoxesByPredicate(
    contractHash: ContractHash,
    f: List[AssetBox] => Boolean
  ): List[AssetBox] =
    loop[AssetBox](
      List.empty[AssetBox],
      getBoxesIdsByKey(assetBoxesByContractHashKey(contractHash)),
      f
    )

  override def getTokenIssuingBoxes(
    contractHash: ContractHash,
    f: List[TokenIssuingBox] => Boolean
  ): List[TokenIssuingBox] =
    loop[TokenIssuingBox](
      List.empty[TokenIssuingBox],
      getBoxesIdsByKey(tokenIssuingBoxesByContractHashKey(contractHash)),
      f
    )

  override def getDataBoxes(
    contractHash: ContractHash,
    f: List[DataBox] => Boolean
  ): List[DataBox] = loop[DataBox](
    List.empty[DataBox],
    getBoxesIdsByKey(dataBoxesByContractHashKey(contractHash)),
    f
  )

  override def getBalancesByContractHash(contractHash: ContractHash): Map[TokenId, Amount] =
    levelDb
      .get(hashToTokens(contractHash))
      .map(_.grouped(32).map(id => id -> getTokenBalanceByContractHash(contractHash, id)).toMap)
      .getOrElse(Map.empty)

  override def getTokenBalanceByContractHash(contractHash: ContractHash, tokenId: TokenId): Amount =
    levelDb.get(tokenKeyByContractHash(contractHash, tokenId)).map(Longs.fromByteArray).getOrElse(0L)

  override def contains(id: ADKey): Boolean = getBoxById(id).isDefined

  override def updateWallet(
    modifierId: ModifierId,
    newBxs: List[EncryBaseBox],
    spentBxs: List[EncryBaseBox],
    intrinsicTokenId: ADKey
  ): Unit = {
    val boxesToInsert: List[EncryBaseBox] = newBxs.filterNot(spentBxs.contains)

    def balanceSheetFunction(list: List[EncryBaseBox], x: Long = -1) =
      BalanceCalculator.balanceSheet1(list, intrinsicTokenId).map {
        case (hash: ContractHash, idToAmount: Map[TokenId, Amount]) =>
          Algos.encode(hash) -> idToAmount.map {
            case (id: TokenId, amount: Amount) => Algos.encode(id) -> (x * amount)
          }
      }

    val balancesToInsert: List[(VersionalLevelDbKey, VersionalLevelDbValue)] = {
      val toAddBalances: Map[String, Map[String, Amount]]    = balanceSheetFunction(newBxs, 1L)
      val toRemoveBalances: Map[String, Map[String, Amount]] = balanceSheetFunction(spentBxs)
      val currentBalances: Map[String, Map[String, Amount]] =
        getBalances.map {
          case (hash: ContractHash, idToAmount: Map[TokenId, Amount]) =>
            Algos.encode(hash) -> idToAmount.map {
              case (id: TokenId, amount: Amount) => Algos.encode(id) -> amount
            }
        }
      (toAddBalances |+| toRemoveBalances |+| currentBalances).flatMap {
        case (hash: String, idToAmount: Map[String, Amount]) =>
          val decodedHash: Array[Byte] = Algos.decode(hash).get
          val tokenIdsToUpdate: (VersionalLevelDbKey, VersionalLevelDbValue) =
            hashToTokens(decodedHash) -> VersionalLevelDbValue @@ (idToAmount.keys.toSet ++ getTokenIds(
              decodedHash
            ).map(Algos.encode).toSet)
              .flatMap((id: String) => Algos.decode(id).get)
              .toArray
          val tokenIdUpdatedAmount: List[(VersionalLevelDbKey, VersionalLevelDbValue)] = idToAmount.map {
            case (id: String, amount: Amount) =>
              tokenKeyByContractHash(decodedHash, Algos.decode(id).get) ->
                VersionalLevelDbValue @@ Longs.toByteArray(amount)
          }.toList

          tokenIdsToUpdate :: tokenIdUpdatedAmount
      }.toList
    }

    val boxesIdsToContractHashToInsert: List[(VersionalLevelDbKey, VersionalLevelDbValue)] = {
      def updatedFunction(hashToBxIds: Map[String, Set[String]], nextHash: String, key: VersionalLevelDbKey) =
        hashToBxIds.updated(nextHash,
                  getBoxesIdsByKey(key)
                    .filterNot(l => spentBxs.exists(_.id sameElements l))
                    .map(Algos.encode)
                    .toSet)
      val (
        assetsFromDb: Map[String, Set[String]],
        dataFromDB: Map[String, Set[String]],
        tokensFromDB: Map[String, Set[String]]
      ) = getAllWallets.foldLeft(
        Map.empty[String, Set[String]],
        Map.empty[String, Set[String]],
        Map.empty[String, Set[String]]
      ) {
        case ((hashToAssetIds, hashToDataIds, hashToTokenIds), nextHash) =>
          val nextHashEncoded: String = Algos.encode(nextHash)
          (updatedFunction(hashToAssetIds, nextHashEncoded, assetBoxesByContractHashKey(nextHash)),
           updatedFunction(hashToDataIds, nextHashEncoded, dataBoxesByContractHashKey(nextHash)),
           updatedFunction(hashToTokenIds, nextHashEncoded, tokenIssuingBoxesByContractHashKey(nextHash)))
      }
      val (
        hashToAssetBoxes: Map[String, Set[String]],
        hashToDataBoxes: Map[String, Set[String]],
        hashToTokenBoxes: Map[String, Set[String]]
      ) = boxesToInsert.foldLeft(
        Map.empty[String, Set[String]],
        Map.empty[String, Set[String]],
        Map.empty[String, Set[String]]
      ) {
        case ((assets, data, token), nextBox: AssetBox) =>
          val hash = Algos.encode(nextBox.proposition.contractHash)
          (assets |+| Map(hash -> Set(Algos.encode(nextBox.id))), token, data)
        case ((assets, data, token), nextBox: DataBox) =>
          val hash = Algos.encode(nextBox.proposition.contractHash)
          (assets, data |+| Map(hash -> Set(Algos.encode(nextBox.id))), token)
        case ((assets, data, token), nextBox: TokenIssuingBox) =>
          val hash = Algos.encode(nextBox.proposition.contractHash)
          (assets, data, token |+| Map(hash -> Set(Algos.encode(nextBox.id))))
      }

      def hashToBxsIdsToDB(
        typeToDb: Map[String, Set[String]],
        hashType: Map[String, Set[String]],
        key: ContractHash => VersionalLevelDbKey
      ): List[(VersionalLevelDbKey, VersionalLevelDbValue)] =
        (typeToDb |+| hashType).map {
          case (hash: String, value: Set[String]) =>
            key(hash) -> VersionalLevelDbValue @@ value
              .flatMap(k => Algos.decode(k).get)
              .toArray
        }.toList

      val newAssetsToDB: List[(VersionalLevelDbKey, VersionalLevelDbValue)] =
        hashToBxsIdsToDB(assetsFromDb, hashToAssetBoxes, assetBoxesByContractHashKey)
      val newDataToDB: List[(VersionalLevelDbKey, VersionalLevelDbValue)] =
        hashToBxsIdsToDB(dataFromDB, hashToDataBoxes, tokenIssuingBoxesByContractHashKey)
      val newTokenToDB: List[(VersionalLevelDbKey, VersionalLevelDbValue)] =
        hashToBxsIdsToDB(tokensFromDB, hashToTokenBoxes, dataBoxesByContractHashKey)
      newAssetsToDB ::: newDataToDB ::: newTokenToDB
    }

    val toInsertBoxes: List[(VersionalLevelDbKey, VersionalLevelDbValue)] =
      boxesToInsert.map(box => (VersionalLevelDbKey @@ box.id.untag(ADKey)) -> VersionalLevelDbValue @@ box.bytes)

    val toRemoveBoxes: List[VersionalLevelDbKey] = spentBxs.map(l => VersionalLevelDbKey @@ l.id.untag(ADKey))

    levelDb.insert(
      LevelDbDiff(
        LevelDBVersion @@ modifierId.untag(ModifierId),
        balancesToInsert ::: boxesIdsToContractHashToInsert ::: toInsertBoxes,
        toRemoveBoxes
      )
    )
  }

  override def rollback(modId: ModifierId): Unit = levelDb.rollbackTo(LevelDBVersion @@ modId.untag(ModifierId))

  override def close(): Unit = levelDb.close()

  private val ASSET_BOXES_BYTES: Array[Byte] = "ASSET_BOXES_BY_USER_KEY".getBytes()

  private val TOKEN_ISSUING_BOXES_BYTES: Array[Byte] = "TOKEN_ISSUING_BOXES_BYTES".getBytes()

  private val DATA_BOXES_BYTES: Array[Byte] = "DATA_BOXES_BYTES".getBytes()

  private val CONTRACT_HASH_TOKEN_IDS: Array[Byte] = "CONTRACT_HASH_TOKEN_IDS".getBytes()

  private val CONTRACT_HASH_ACCOUNTS: VersionalLevelDbKey = VersionalLevelDbKey @@ Algos.hash("CONTRACT_HASH_ACCOUNTS")

  def hashToTokens(userHash: ContractHash): VersionalLevelDbKey =
    VersionalLevelDbKey @@ Algos.hash(userHash ++ CONTRACT_HASH_TOKEN_IDS)

  private def assetBoxesByContractHashKey(userHash: ContractHash): VersionalLevelDbKey =
    VersionalLevelDbKey @@ Algos.hash(userHash ++ ASSET_BOXES_BYTES)

  private def tokenIssuingBoxesByContractHashKey(userHash: ContractHash): VersionalLevelDbKey =
    VersionalLevelDbKey @@ Algos.hash(userHash ++ TOKEN_ISSUING_BOXES_BYTES)

  private def dataBoxesByContractHashKey(userHash: ContractHash): VersionalLevelDbKey =
    VersionalLevelDbKey @@ Algos.hash(userHash ++ DATA_BOXES_BYTES)

  private def tokenKeyByContractHash(userHash: ContractHash, tokenId: TokenId): VersionalLevelDbKey =
    VersionalLevelDbKey @@ Algos.hash(userHash ++ tokenId)
}

object WalletDBImpl {}
