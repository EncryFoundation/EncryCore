
# State

State is the result of the sequential modifier applying.

    version: VersionId                                      // ModifierId of the last applied persistent modifier.
    
    stateStorage:
        persistentProver: AVLProver[BoxId, BoxSerialized]   // Stores only boxes.
        stateIndex: Store[K, V]                             // Stores contextual information such as `lastAppliedHeader`
                                                            // `stateHeight`, etc.
        
        rollback(to: VersionId) = {
            persistentProver.rollback(to)
            stateIndex.rollback(to)
        }
        
    rollback(to: VersionId) = stateStorage.rollback(to)
    
    getBox(id: ModifierId)
    
    getContext()
    
    validate(tx: Transaction)

# History

    historyStorage:
        historyIndex: Store[K, V]
        objectsStore: FileStore[ModifierId, ModifierSerialized]
        
    getModifier(id: ModifierId)
    
# Mempool

    unconfirmedPool: TrieMap[key(TransactionId), Transaction]

# Vault

    valutStorage:
        walletStore: Store[K, V]
