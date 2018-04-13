# EncryCore protocol specification

## Introduction

Encry is supporting multiple security models. In addition to full node mode, which is similar to Bitcoin fullnode, 
Encry reference implementation supports Light-SPV, Light-Fullnode and Clipped-Fullnode modes.

## Full node mode
A full node is storing all the full blocks since genesis block (like in Bitcoin). Full node checks PoW's,
linking structure correctness (parent block id), and all the transactions in all the blocks.
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
    
   * Now we have BlockPayload: all transactions corresponding to some Header
   
    if(History.apply(BlockPayload) == Success(ProgressInfo)) {
        if(!isInitialBootstrapping) Broadcast INV for BlockTransactions
        /* We should notify our neighbours, that now we have all the transactions
        State apply modifiers (may be empty for block in a fork chain)
        and generate ADProofs for them.
        (e.g. every 30000 blocks like in Ethereum). */
        if (State().apply(ProgressInfo) == Success((newState, ADProofs))) {
          if("mode"="full" || "mode"=="pruned-full") ADProofs.foreach ( ADProof => History.apply(ADProof))
          if("mode"=="pruned-full" || "mode"=="light-full") drop BlockTransactions and ADProofs older than BlocksToKeep
        } else {
          // Drop Header from history, because it's transaction sequence is not valid
          History.drop(BlockTransactions.headerId)
        }
    } else {
        blacklist peer who sent header
    }

## Transaction model

    EncryTransaction (#src/main/scala/encry/modifiers/mempool/EncryTransaction.scala)
        accountPubKey
        fee
        timestamp
        signature
        unlockers:
            Unlocker
                boxId
                proofOpt:
                    Proof (Abstract type)
                    -> Signature25519 extends Proof
                    -> MultiProof extends Proof
        directives:
            Directive (Abstract type)
            -> CoinbaseDirective extends Directive
                idx                     // Index of directive in directives vector in transaction
                height                  // Height after coins created by directive can be spent
            -> TransaferDirective extends Directive
                idx
                address                 // Recipient address
                amount
            -> ScriptedAsset extends Directive
                idx
                script                  // Script to be used as the proposition
                amount
