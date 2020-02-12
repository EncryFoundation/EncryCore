package encry.view.wallet

import com.typesafe.scalalogging.StrictLogging
import encry.settings.EncryAppSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDB
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.VersionalLevelDbKey
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.modifiers.state.box.{AssetBox, DataBox, EncryBaseBox, TokenIssuingBox}
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ADKey, ModifierId}
import org.encryfoundation.prismlang.compiler.CompiledContract.ContractHash

class WalletDBImpl private (
  levelDb: VersionalLevelDB,
  settings: EncryAppSettings
) extends WalletDB
    with StrictLogging with AutoCloseable {

  override def getAllWallets: List[ContractHash] = ???

  override def getBoxById(id: ADKey): Option[EncryBaseBox] = ???

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
    newBxs: Seq[EncryBaseBox],
    spentBxs: Seq[EncryBaseBox],
    intrinsicTokenId: ADKey
  ): Unit = ???

  override def rollback(modId: ModifierId): Unit = ???

  override def close(): Unit = ???

  private val ASSET_BOXES_BYTES: Array[Byte] = "ASSET_BOXES_BY_USER_KEY".getBytes()

  private val TOKEN_ISSUING_BOXES_BYTES: Array[Byte] = "TOKEN_ISSUING_BOXES_BYTES".getBytes()

  private val DATA_BOXES_BYTES: Array[Byte] = "DATA_BOXES_BYTES".getBytes()

  private val CONTRACT_HASH_ACCOUNTS: VersionalLevelDbKey = VersionalLevelDbKey @@ Algos.hash("CONTRACT_HASH_ACCOUNTS")

  private def assetBoxesByContractHashKey(userHash: ContractHash): VersionalLevelDbKey =
    VersionalLevelDbKey @@ Algos.hash(userHash ++ ASSET_BOXES_BYTES)

  private def tokenIssuingBoxesByContractHashKey(userHash: ContractHash): VersionalLevelDbKey =
    VersionalLevelDbKey @@ Algos.hash(userHash ++ TOKEN_ISSUING_BOXES_BYTES)

  private def dataBoxesByContractHashKey(userHash: ContractHash): VersionalLevelDbKey =
    VersionalLevelDbKey @@ Algos.hash(userHash ++ DATA_BOXES_BYTES)

  private def tokenKeyByContractHash(userHash: ContractHash, tokenId: TokenId): VersionalLevelDbKey =
    VersionalLevelDbKey @@ Algos.hash(userHash ++ tokenId)
}

object WalletDBImpl {

}