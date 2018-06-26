| Route | get/post | Args | Output |
|-------|----------|------|--------|
|/info                               | get  |  |  |
|/transactions                       | post |  |  |
|/transactions/{id}                  | get  |  |  |
|/transactions/unconfirmed           | get  |* |  |
|/peers/all                          | get  |  |  |
|/peers/connected                    | get  |  |  |
|/peers/connect                      | post |  |  |
|/peers/blacklisted                  | get  |  |  |
|/utils/seed                         | get  |  |  |
|/utils/hash/blake2b                 | get  |  |  |
|/state/boxes/{address}              | get  |  |  |
|/state/portfolio/{address}          | get  |  |  |
|/history                            | get  |  |  |
|/history/lastHeaders/{n}            | get  |  |  |
|/history/at/{n}                     | get  |  |  |
|/history/{modifierId}/header        | get  |  |  |
|/history/{modifierId}/transactions  | get  |  |  |
|/history/candidateBlock             | get  |  |  |
|/api-docs/swagger.conf              | get  |  |  |
  
* optional integer parameters `limit` and `offset`  
  
# Get
## 1. /info 
_output_:  
```
{  
  "name" : "node4",  
  "stateType" : "utxo",  
  "difficulty" : "66",  
  "bestFullHeaderId" : "13zJHL22zDivD8Yb48UeSx84TLsKsDSJvRyXAoiLzsHs",  
  "bestHeaderId" : "A3akC5p7feXwzCmr9cqKvgidRWAVJx39z6cDJ1xH81g",  
  "peersCount" : 3,  
  "unconfirmedCount" : 0,  
  "previousFullHeaderId" : "DmrZeUmPGhshsMbMskn5NbKWkGAyeBJK5TFniDHTZWU",  
  "fullHeight" : 910,  
  "headersHeight" : 2964,  
  "stateVersion" : "R1K4ELvPA4cUWNUcsnXJWG8fz3PPkNU8zepwxSHANoD",  
  "uptime" : 425890,  
  "isMining" : false  
}
```
