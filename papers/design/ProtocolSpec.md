# EncryCore protocol specification

## Introduction

Encry is supporting multiple security models. In addition to full node mode, which is similar to Bitcoin fullnode, 
Encry reference implementation supports Light-SPV, Light-Fullnode and Pruned-Fullnode modes.

## Full node mode
Like in Bitcoin, a full node is storing all the full blocks since genesis block. Full node checks proofs of work, 
linking structure correctness (parent block id, interlink elements), and all the transactions in all the blocks. 
A fullnode is storing all the full blocks forever. It is also holding full UTXO set to be able to validate an arbitrary transaction.
The only optimization a fullnode is doing is that is is skipping downloading and checking AD-transformation block part 
(see below in the "Light-Fullnode" section). For the full node regime, modifiers precessing workflow is as follows:

   * Send EncrySyncInfo message to connected peers.
   * Get response with INV message, containing ids of blocks, better than our best block.
   * Request headers for all ids from 2.
   * On receiving header:
   
    if(history.apply(header).isSuccess) {
        if(!isInitialBootstrapping) Broadcast INV for this header   
        Request transaction ids from this block
    } else {
        blacklist peer
    }
     
   * On receiving transaction ids from header:

    transactionIdsForHeader.filter(txId => !MemPool.contains(txId)).foreach { txId => 
        request transaction with txId
    }
    
   * On receiving a transaction:
   
    if(Mempool.apply(transaction).isSuccess) {
       if(!isInitialBootstrapping) Broadcast INV for this transaction
       Mempool.getHeadersWithAllTransactions { BlockTransactions =>
          GOTO 7
       }
    }
    
   * Now we have BlockTransactions: all transactions corresponding to some Header
   
    if(History.apply(BlockTransactions) == Success(ProgressInfo)) {
        if(!isInitialBootstrapping) Broadcast INV for BlockTransactions
        /* We should notify our neighbours, that now we have all the transactions
        State apply modifiers (may be empty for block in a fork chain)
        and generate ADProofs for them.
        TODO requires different interface from scorex-core,
        because it should return ADProofs
        TODO when mininal state apply Progress info,
        it may also create UTXOSnapshot
        (e.g. every 30000 blocks like in Ethereum). */
        if (State().apply(ProgressInfo) == Success((newState, ADProofs))) {
          if("mode"="full" || "mode"=="pruned-full") ADProofs.foreach ( ADProof => History.apply(ADProof))
          if("mode"=="pruned-full" || "mode"=="light-full") drop BlockTransactions and ADProofs older than BlocksToKeep
        } else {
          //Drop Header from history, because it's transaction sequence is not valid
          History.drop(BlockTransactions.headerId)
        }
    } else {
        blacklist peer who sent header
    }
