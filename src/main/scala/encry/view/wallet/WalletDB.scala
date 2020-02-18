package encry.view.wallet

import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.VersionalLevelDbKey
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.modifiers.state.box.{ AssetBox, DataBox, EncryBaseBox, TokenIssuingBox }
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ ADKey, ModifierId }
import org.encryfoundation.prismlang.compiler.CompiledContract.ContractHash

trait WalletDB {

  def getBoxesIdsByKey(key: VersionalLevelDbKey): List[ADKey]

  def getAllWallets: List[ContractHash]

  def getBoxById(id: ADKey): Option[EncryBaseBox]

  def getAssetBoxesByPredicate(contractHash: ContractHash, f: List[AssetBox] => Boolean): List[AssetBox]

  def getTokenIssuingBoxes(contractHash: ContractHash, f: List[TokenIssuingBox] => Boolean): List[TokenIssuingBox]

  def getDataBoxes(contractHash: ContractHash, f: List[DataBox] => Boolean): List[DataBox]

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
