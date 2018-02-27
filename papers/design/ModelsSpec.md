
## Account Model
Represents the owner of the Public/Private key pair.

    address: String             // Derived from PublicKey. Base58Check encoded string.  

## Base Proposition Model
Defines requirements for box unlocking.

    ???: Array[Byte]            // pubKeyBytes, Script, etc.
    
## Context Model
Context holds the external information. (required for box unlocking)

    height: Int                 // Chain height at the moment of transaction validation.
    transaction: Transaction    // Transaction which tries to spend particular box.
    
## Base Proof Model
Proof holds the data corresponding to particular Proposition. 
For instance, when we have PublicKey as a Proposition, proof should contain signature byes.
(required for box unlocking)

    proofBytes: Array[Byte]
    
## Base Box Model

    id: ADKey
    proposition: Proposition
    
Key method:

    unlockTry(proof: Proof, context: Context)   // Executes manipulations defined by proposition type 
                                                // with data provided by `proof` and `context` to figure out
                                                // if the box can be unlocked.
                                                
## Base Directive Model
Defines operation on state. 

    boxes: [Box]                                // Transforms directive into boxes.
    cost: Amount                                // Will be used for `transaction.fee` minimal amount calculating.
    
## Unlocker
Holds the boxId/proof pair.

    boxId: ADKey
    proof: Option[Proof]

## Base Transaction Model
Atomic state modifier.

    senderPubKey: PublicKey
    senderSignature: Signature
    fee: Amount
    useBoxes: [Unlocker]                        // If `proof` is None, then `senderSignature` is used as proof.
    directives: [Directive]                     // List of state modifications which should be applied atomically.
