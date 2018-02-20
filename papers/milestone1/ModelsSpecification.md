
## Base Proposition Model
Describes requirements for its unlocking.

    baseData: Array[Byte] (pubKeyBytes, Script, etc)
    address: String (derived from baseData)
    
## Context Model
Context holds the external information. (required for box unlocking)

    height: Int     // Chain height at the moment of transaction validation.
    transaction: Transaction    // Transaction which tries to spend the 
    
## Base Proof Model
Proof holds the data corresponding to particular Proposition. 
For instance, when we have PublicKey as a Proposition, proof should contain signature byes.
(required for box unlocking)

    proofBytes: Array[Byte]
    
## Base Box Model

    id: ADKey
    proposition: Proposition
    
Key method:

    unlockTry(proof: Proof, context: Context): Try[Unit]

## Base Transaction Model
    senderPubKey: PublicKey
    senderSignature: Signature
    useBoxes: [(BoxId, Proof)]
    createBoxes: [(Proposition, Amount)]