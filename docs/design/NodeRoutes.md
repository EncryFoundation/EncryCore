| Route | get/post | Args |
|-------|----------|------|
|/info                               | get  | | 
|/transactions                       | post | | 
|/transactions/{id}                  | get  | | 
|/transactions/unconfirmed           | get  |*| 
|/peers/all                          | get  | |
|/peers/connected                    | get  | |
|/peers/connect                      | post | |
|/utils/seed                         | get  | |
|/utils/hash/blake2b                 | post | |
|/state/boxes/{address}              | get  | |
|/history                            | get  |*|
|/history/lastHeaders/{n}            | get  | |
|/history/at/{n}                     | get  | |
|/history/{modifierId}/header        | get  | |
|/history/{modifierId}/transactions  | get  | |
|/history/candidateBlock             | get  | |
|/api-docs/swagger.conf              | get  | |
  
* optional integer parameters `limit` and `offset`  
  
# Get
## 1. /info
Usage:
    ` GET - http://nodeIp:nodePort/info`
    
_Output_:  
```
{  
  "name" : "testnet-01",  
  "stateType" : (utxo / digest),  
  "difficulty" : "66",  
  "bestFullHeaderId" : "fe71d43ca8f7b37912c5581353e3b426d294fb1bdb60050161c4ba4603b6f50b",  
  "bestHeaderId" : "fe71d43ca8f7b37912c5581353e3b426d294fb1bdb60050161c4ba4603b6f50b",  
  "peersCount" : 9,  
  "unconfirmedCount" : 0,  
  "previousFullHeaderId" : "23a50fafdc9a10761d4d38ff840df6e3266905239acedd5d3ef9e803bb0640a1",  
  "fullHeight" : 167843,  
  "headersHeight" : 167843,  
  "stateVersion" : "fe71d43ca8f7b37912c5581353e3b426d294fb1bdb60050161c4ba4603b6f50b",  
  "uptime" : 425890,
  "storage" : LevelDB(Write / Read) Postgres(Write / Read),
  "isConnectedWithKnownPeers": (true / false),
  "isMining" : (true / false),
  "knownPeers" : []
}
```

Description: Get info about node's last state. This request shows last actual info about node's last inner state, where:

    1) 'name' - current node name
    2) 'stateType' - current state's type
        'utxo' - keep full utxo set, that allows to validate arbitrary block and generate ADProofs
        'digest' - keep state root hash only and validate transactions via ADProofs
    3) 'difficulty' - current mining difficulty
    4) 'bestFullHeaderId' - ID of current best full block's header in node's chain
    5) 'bestHeaderId' - ID of current best header in chain in node's chain
    6) 'peersCount' - number of nodes connected to this
    7) 'unconfirmedCoun' - number of transactions in mempool
    8) 'previousFullHeaderId' - ID of previous best full block's header
    9) 'fullHeight' - number of blocks in current node's chain
    10)'headersHeight' - number of headers in current node's chain
    11)'stateVersion' - special id of last block, which has been written into IODb
    12)'uptime' - time, from node's start
    13)'storage' - info about storage with which we interact
        (LevelDB / Postgres) write - if we write into (LevelDB / Postgres)
        (LevelDB / Postgres) read - if we restore from (LevelDB / Postgres)
    14)'isConnectedWithKnownPeers' - 
    15)'isMining' - info about mining state
        true - mining is on
        false - mining is off
    16)'knownPeers' - peers, which this node knows about from config
    
## 2. Transactions
Usage:

    1)`GET - http://nodeIp:nodePort/transactions/unconfirmed`
    2)`GET - http://nodeIp:nodePort/transactions/{Id}`
    3)`POST - http://nodeIp:nodePort/transactions`
Output:
```

```
Description: 

    1) Get list of unconfirmed transactions in mempool of this node. This transactions are waiting for applying.
    2) Get info about single transaction with unique `Id`. Soon this transaction will be applied or rejected
    3) Send handmade transaction to the node mempool
     
## 3. Peers
Usage:

    1)`GET - http://nodeIp:nodePort/peers/all`
    2)`GET - http://nodeIp:nodePort/peers/connected`
Output:
1) 
```
  [
   {
     "address": "ip:port",
     "name": null,
     "connectionType": null
   }
  ]
```
Description 
 
    1) Get view about all peers which node knows about
    2) Get view about peers node connects with in current time

## 4. Utils
Usage: `/utils/seed` or `/utils/seed/{SeedSize}`

_output_: `"b5c33680b4cd357467d748c2d73bf56af958b9a6e95f53209b0e15f9c2852e73"`

Description: returns random Byte58 string for random value of `SeedSize` or 32 (default) bytes.

_entity_: Any text or json

_output_: `"3etZenM7MQb4w45xmhAgQzmMBtdA7QPV8nD6uBDgQ2wi"`

## 5. State
`/state/boxes/{address}`
_Output example_ :
```
[
    {
      "nonce" : -8506939166499002754,
      "id" : "8fkABbU9oR6MJyuQFp2usiXGc8fdUufjY1XYEqeuUYN",
      "tokenId" : null,
      "type" : 1,
      "proposition" : {
        "script" : "SNpbTW2noppcART1wHe6mHLqYtajiiWwFvXbnaP35kZShimrfKEjoXDpsbmrNc3ToVLQHXcwfGzG5XdKHPdXXQUgQud42N6h4M4UKttcwxJT5GfurTQJDq4gKCdVVqJknFrfvDLr5azFacz8217Gsmj2hge1m1B7mahDHRzZ3Y471pwHrDGspbiuyQCvvd7jiWMupmxKJ8483wVjjXUFck4Qg94SoYJmnubjNAuwv1DJmg"
      },
      "value" : 1969700
    },
    ...
    {
      "nonce" : 7542729214514099406,
      "id" : "8Lraq7YnZL3Me2KTGgxuAT4cCaGuQtdGEd2Xy2uvBR8",
      "tokenId" : null,
      "type" : 1,
      "proposition" : {
        "script" : "SNpbTW2noppcART1wHe6mHLqYtajiiWwFvXbnaP35kZShimrfKEjoXDpsbmrNc3ToVLQHXcwfGzG5XdKHPdXXQUgQud42N6h4M4UKttcwxJT5GfurTQJDq4gKCdVVqJknFrfvDLr5azFacz8217Gsmj2hge1m1B7mahDHRzZ3Y471pwHrDGspbiuyQCvvd7jiWMupmxKJ8483wVjjXUFck4Qg94SoYJmnubjNAuwv1DJmg"
      },
      "value" : 1989800
    }
]
```

Description 

    1) Show view with all boxes which current 'address' able to spend
    
    a) nonce - number for POW
    b) id - ID of current box
    c) tokenID - ID of current token 
    d) type - type of this box ( Assert, Data, Token )
    e) proposition - script's hash, which lock this box
    f) value - monetary value of current box

## 6. History
`/history/`
`/history/at/{0}`
`/history/lastHeaders/{number}`
`/history/{modifierId}/header`
`/history/{modifierId}/transactions`
`history/candidateBlock`

Description: Different views, which show information about chain in node's state

    1) `/history/` - returns first 50 header's IDs
    2) `/history/at/{0}` - header's ID at {_} height
    3) `/history/lastHeaders/{number}` - number of last {__} headers in node's chain
    4) `/history/{modifierId}/header` - header's view. Shows info about current header. 
    5) `/history/{modifierId}/transactions` - list of transactions, which belong to block with header {__} ID.
    6) `history/candidateBlock` - information about block which will be the basis of the next mining block

_Output example_:
```
{
  "isMining" : true,
  "candidateBlock" : "None"
}
```

## 7. Swagger

`/api-docs/swagger.conf`

Description: Swagger API