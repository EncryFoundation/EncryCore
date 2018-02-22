
## Account Model
Represents the owner of the Public/Private key pair.

    publicKey: PublicKey

## Base Proposition Model
Defines requirements for box unlocking.

    baseData: Array[Byte]       // pubKeyBytes, Script, etc.
    address: String             // Derived from baseData.
    
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

## Base Transaction Model

    senderPubKey: PublicKey
    senderSignature: Signature
    useBoxes: [(BoxId, Option[Proof])]          // If `proof` is None, then `senderSignature` is used as proof.
    createBoxes: [(Proposition, Amount)]
