
## Generic Proposition Model
    baseData: Array[Byte] (pubKeyBytes, Script, etc)
    address: String (derived from baseData)
    
## Generic Box Model
    id: ADKey
    proposition: Proposition

## Generic Transaction Model
    senderPubKey: PublicKey
    senderSignature: Signature
    useBoxes: [(BoxId, BoxKey)]
    createBoxes: [(Proposition, Amount)]