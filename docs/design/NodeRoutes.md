| Route | get/post | Args |
|-------|----------|------|
|/info                               | get  |  |
|/transactions                       | post |  |
|/transactions/{id}                  | get  |  |
|/transactions/unconfirmed           | get  |* |
|/peers/all                          | get  |  |
|/peers/connected                    | get  |  |
|/peers/connect                      | post |  |
|/peers/blacklisted                  | get  |  |
|/utils/seed                         | get  |  |
|/utils/hash/blake2b                 | post |  |
|/state/boxes/{address}              | get  |  |
|/state/portfolio/{address}          | get  |  |
|/history                            | get  |  |
|/history/lastHeaders/{n}            | get  |  |
|/history/at/{n}                     | get  |  |
|/history/{modifierId}/header        | get  |  |
|/history/{modifierId}/transactions  | get  |  |
|/history/candidateBlock             | get  |  |
|/api-docs/swagger.conf              | get  |  |
  
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

## 2. History
`/history/`
`/history/at/{0}`
`/history/lastHeaders/{number}`
Description: returns list of header Ids

`/history/{modifierId}/header`
`/history/{modifierId}/transactions`


`history/candidateBlock`
_Output example_:
```
{
  "isMining" : true,
  "candidateBlock" : "None"
}
```

## 3. Transactions
Usage:
    `transactions/unconfirmed`
    `transactions/{Id}`
Output:
```

```
Description: Get list of unconfirmed transactions or transaction with `Id` _[how to get?]_

## 4. State
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

`/state/portfolio/{address}`

## 5. Peers.
Usage:
    `/peers/all`
    `/peers/connected`
    `/peers/blacklisted`
Description: _[Always returns [] (???)]_

## 6. Utils
Usage: `/utils/seed` or `/utils/seed/{SeedSize}`
_output_: `"VNpiGdoVzSekNqgywWZbjp"`
Description: returns random Byte58 string for random value of `SeedSize` or 32 (default) bytes.


# Post
## 1. /utils/hash/blake2b
_entity_: Any text or json
_output_: `"3etZenM7MQb4w45xmhAgQzmMBtdA7QPV8nD6uBDgQ2wi"`