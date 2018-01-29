package encry.view.wallet

import encry.modifiers.EncryPersistentModifier
import encry.modifiers.mempool.EncryBaseTransaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.transaction.wallet.Vault

trait BaseVault extends Vault[Proposition, EncryBaseTransaction, EncryPersistentModifier, BaseVault]
