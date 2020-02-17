package encry.view.wallet

import cats.syntax.semigroup._
import cats.instances.long._
import cats.instances.map._
import cats.instances.set._
import cats.instances.list._
import com.google.common.primitives.Longs
import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.storage.levelDb.versionalLevelDB.{ LevelDbDiff, VersionalLevelDB, VersionalLevelDBCompanion }
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.{
  LevelDBVersion,
  VersionalLevelDbKey,
  VersionalLevelDbValue
}
import encry.utils.BalanceCalculator
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.modifiers.state.box.{
  AssetBox,
  DataBox,
  EncryBaseBox,
  EncryBox,
  MonetaryBox,
  TokenIssuingBox
}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ ADKey, ModifierId }
import org.encryfoundation.prismlang.compiler.CompiledContract.ContractHash
import supertagged.@@

import scala.collection.immutable

class WalletDBImpl private (
  levelDb: VersionalLevelDB,
  settings: EncryAppSettings
) extends WalletDB
    with StrictLogging
    with AutoCloseable {

  override def getAllWallets: List[ContractHash] = ???

  def getBalances: Map[ContractHash, Map[TokenId, Amount]] = ???

  override def getBoxById(id: ADKey): Option[EncryBaseBox] = ???

  def getTokenIds(hash: ContractHash): List[TokenId] = ???

  def assetKeysByHash(hash: ContractHash): List[ADKey] = ???
  def tokenKeysByHash(hash: ContractHash): List[ADKey] = ???
  def dataKeysByHash(hash: ContractHash): List[ADKey]  = ???

  override def getAssetBoxesByPredicate(
    contractHash: ContractHash,
    f: List[AssetBox] => Boolean
  ): List[AssetBox] = ???

  override def getTokenIssuingBoxes(
    contractHash: ContractHash,
    f: List[TokenIssuingBox] => Boolean
  ): List[TokenIssuingBox] = ???

  override def getDataBoxes(
    contractHash: ContractHash,
    f: List[DataBox] => Boolean
  ): List[DataBox] = ???

  override def getBalancesByContractHash(contractHash: ContractHash): Map[TokenId, Amount] = ???

  override def getTokenBalanceByContractHash(contractHash: ContractHash, tokenId: TokenId): Amount = ???

  override def contains(id: ADKey): Boolean = ???

  override def updateWallet(
    modifierId: ModifierId,
    newBxs: List[EncryBaseBox],
    spentBxs: List[EncryBaseBox],
    intrinsicTokenId: ADKey
  ): Unit = {
    val boxesToInsert = newBxs.filterNot(spentBxs.contains)
    val newBalances: Map[ContractHash, Map[TokenId, Amount]] = {
      val toAdd = BalanceCalculator.balanceSheet1(newBxs, intrinsicTokenId).map {
        case (hash, idToAmount) =>
          Algos.encode(hash) -> idToAmount.map {
            case (id, amount) => Algos.encode(id) -> amount
          }
      }
      val toRemove = BalanceCalculator.balanceSheet1(spentBxs, intrinsicTokenId).map {
        case (hash, idToAmount) =>
          Algos.encode(hash) -> idToAmount.map {
            case (id, amount) => Algos.encode(id) -> amount
          }
      }
      val current = getBalances.map {
        case (hash, idToAmount) =>
          Algos.encode(hash) -> idToAmount.map {
            case (id, amount) => Algos.encode(id) -> amount
          }
      }
      (toAdd |+| toRemove |+| current).map {
        case (str, stringToAmount) =>
          Algos.decode(str).get -> stringToAmount.map {
            case (str, amount) => Algos.decode(str).get -> amount
          }
      }
    }
    val accs: List[ContractHash] = getAllWallets

    val a: (List[AssetBox], List[DataBox], List[TokenIssuingBox]) =
      boxesToInsert.foldLeft(List.empty[AssetBox], List.empty[DataBox], List.empty[TokenIssuingBox]) {
        case ((asset, data, token), nextBox: AssetBox) =>
          (nextBox :: asset, data, token)
        case ((asset, data, token), nextBox: DataBox) =>
          (asset, nextBox :: data, token)
        case ((asset, data, token), nextBox: TokenIssuingBox) =>
          (asset, data, nextBox :: token)
        case ((asset, data, token), _) =>
          (asset, data, token)
      }

    val assetByAcc: Map[String, List[AssetBox]] = a._1.foldLeft(Map.empty[String, List[AssetBox]]) {
      case (hashToBoxes, nextBox) =>
        val hash = Algos.encode(nextBox.proposition.contractHash)
        hashToBoxes |+| Map(hash -> List(nextBox))
    }
    val dataByAcc: Map[String, List[DataBox]] = a._2.foldLeft(Map.empty[String, List[DataBox]]) {
      case (hashToBoxes, nextBox) =>
        val hash = Algos.encode(nextBox.proposition.contractHash)
        hashToBoxes |+| Map(hash -> List(nextBox))
    }
    val tokenByAcc: Map[String, List[TokenIssuingBox]] = a._3.foldLeft(Map.empty[String, List[TokenIssuingBox]]) {
      case (hashToBoxes, nextBox) =>
        val hash = Algos.encode(nextBox.proposition.contractHash)
        hashToBoxes |+| Map(hash -> List(nextBox))
    }

    val assetFromDbCleaned: Map[String, Set[String]] = accs.map { hash =>
      Algos.encode(hash) -> assetKeysByHash(hash)
        .filterNot(l => spentBxs.exists(_.id sameElements (l)))
        .map(Algos.encode)
        .toSet
    }.toMap
    val tokenFromDbCleaned: Map[String, Set[String]] = accs.map { hash =>
      Algos.encode(hash) -> tokenKeysByHash(hash)
        .filterNot(l => spentBxs.exists(_.id sameElements (l)))
        .map(Algos.encode)
        .toSet
    }.toMap
    val dataFromDbCleaned: Map[String, Set[String]] = accs.map { hash =>
      Algos.encode(hash) -> dataKeysByHash(hash)
        .filterNot(l => spentBxs.exists(_.id sameElements (l)))
        .map(Algos.encode)
        .toSet
    }.toMap

    val newAsset: Map[String, Set[String]] = assetFromDbCleaned |+| assetByAcc.map {
      case (str, boxes) => str -> boxes.map(k => Algos.encode(k.id)).toSet
    }
    val newToken: Map[String, Set[String]] = tokenFromDbCleaned |+| tokenByAcc.map {
      case (str, boxes) => str -> boxes.map(k => Algos.encode(k.id)).toSet
    }
    val newData: Map[String, Set[String]] = dataFromDbCleaned |+| dataByAcc.map {
      case (str, boxes) => str -> boxes.map(k => Algos.encode(k.id)).toSet
    }

    val newAssetKeys: List[(VersionalLevelDbKey, VersionalLevelDbValue)] = newAsset.map {
      case (hash, value) =>
        assetBoxesByContractHashKey(Algos.decode(hash).get) -> VersionalLevelDbValue @@ value
          .flatMap(k => Algos.decode(k).get)
          .toArray
    }.toList
    val newDataKeys: List[(VersionalLevelDbKey, VersionalLevelDbValue)] = newData.map {
      case (hash, value) =>
        dataBoxesByContractHashKey(Algos.decode(hash).get) -> VersionalLevelDbValue @@ value
          .flatMap(k => Algos.decode(k).get)
          .toArray
    }.toList
    val newTokenKeys: List[(VersionalLevelDbKey, VersionalLevelDbValue)] = newToken.map {
      case (hash, value) =>
        tokenIssuingBoxesByContractHashKey(Algos.decode(hash).get) -> VersionalLevelDbValue @@ value
          .flatMap(k => Algos.decode(k).get)
          .toArray
    }.toList

    val balancesToInsert: List[(VersionalLevelDbKey, VersionalLevelDbValue)] = newBalances.flatMap {
      case (hash, idToAmount) =>
        val keys                 = idToAmount.keys.map(Algos.encode).toSet
        val curK: Set[String]    = getTokenIds(hash).map(Algos.encode).toSet
        val newKeys: Set[String] = (keys ++ curK)
        val upd: List[(VersionalLevelDbKey, VersionalLevelDbValue)] = idToAmount.map {
          case (id, amount) =>
            tokenKeyByContractHash(hash, id) -> VersionalLevelDbValue @@ Longs.toByteArray(amount)
        }.toList
        val newKeysupd = hashToTokens(hash) -> VersionalLevelDbValue @@ newKeys
          .flatMap(j => Algos.decode(j).get)
          .toArray
        newKeysupd :: upd
    }.toList
    val toIns = LevelDbDiff(
      LevelDBVersion @@ modifierId.untag(ModifierId),
      balancesToInsert ::: newTokenKeys ::: newDataKeys ::: newAssetKeys ::: boxesToInsert.map { box =>
        (VersionalLevelDbKey @@ box.id.untag(ADKey)) -> VersionalLevelDbValue @@ box.bytes
      },
      spentBxs.map(l => VersionalLevelDbKey @@ l.id.untag(ADKey))
    )
    levelDb.insert(toIns)
  }

  override def rollback(modId: ModifierId): Unit = ???

  override def close(): Unit = ???

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
