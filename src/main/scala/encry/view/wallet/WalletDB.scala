package encry.view.wallet

import encry.settings.EncryAppSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDB
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.modifiers.state.box.{ AssetBox, DataBox, EncryBaseBox, TokenIssuingBox }
import org.encryfoundation.common.utils.TaggedTypes.{ ADKey, ModifierId }
import org.encryfoundation.prismlang.compiler.CompiledContract.ContractHash

trait WalletDB {

  def getBoxById(id: ADKey): Option[EncryBaseBox]

  def getAllWallets: List[ContractHash]

  def getAssetBoxesByPredicate(contractHash: ContractHash, f: List[AssetBox] => Boolean): List[AssetBox]

  def getTokenIssuingBoxesByPredicate(
    contractHash: ContractHash,
    f: List[TokenIssuingBox] => Boolean
  ): List[TokenIssuingBox]

  def getDataBoxesByPredicate(contractHash: ContractHash, f: List[DataBox] => Boolean): List[DataBox]

  def getBalancesByContractHash(contractHash: ContractHash): Map[TokenId, Amount]

  def getTokenBalanceByContractHash(contractHash: ContractHash, tokenId: TokenId): Amount

  def contains(id: ADKey): Boolean

  def updateWallet(
    modifierId: ModifierId,
    newBxs: List[EncryBaseBox],
    spentBxs: List[EncryBaseBox],
    intrinsicTokenId: ADKey
  ): Unit

  def rollback(modId: ModifierId): Unit
}

object WalletDB {
  def apply(levelDb: VersionalLevelDB, settings: EncryAppSettings): WalletDB = WalletDBImpl.apply(levelDb, settings)
}
