# EncryCore roadmap

### Milestone 0
- POW Consensus Algo implementation
- Distributed State Machine implementation
- 2 hardcoded types of transactions:
    + PaymentTransaction (Provides user with the ability to transfer intrinsic tokens (ETT))
    + AddPubKeyInfoTransaction (PKI)
- 2 hardcoded types of proposition:
    + PublicKey proposition
    + Height proposition
- Account model implemented as address derived from wallet publicKey
- Multi-Thread miner implementation
- Memory pool implementation:
    + Lazy cleanup mechanism
    + Scheduled cleanup mechanism
- Wallet implementation (Keeps tack of vault-specific information such as balance, available boxes etc.)
    + KeyManager 
- State Index (Accounts state tracking in order to provide quick access to blockchain state though APIs)
- API:
    + History
    + State Index
    + Node Info
    + Transactions
        
### Milestone 1
- Generic Proposition model
- Generic Account model implemented as identifier derived from any proposition
- Adjustable State Index (With the ability to select which types of state events to track)
    
    

