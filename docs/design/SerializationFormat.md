# Data structures

Note, for encoding\decoding of collections recursive length prefixing technique is used (example: \[`xLen`: 0-4; 4-`xLen`\], where `xLen ` = `element.bytes`  - means that each `element.bytes` record in collection is prefixed with its encoded length)

`bytes.len` - length of the whole byte representation of the object
`?` - optional length

## Node view modifiers

This category of modifiers affects blockchain state and thus are persisted

### Header

Block header

ModifierId | 101
-----------|:----
Bytes total| 154 + `difficultySize` + `equihashSolutionSize`

Field name | Bytes distribution
-----------|-------------------
`version[VersionId]`  | 0 - 1
`parentId[ModifierId]`| 1 - 33
`adProofsRoot`        | 33 - 65
`stateRoot`           | 65 - 98
`txsRoot`             | 98 - 130
`timestamp[Long]`     | 130 - 138
`height[Int]`         | 138 - 142
`nonce[Long]`         | 142 - 150
`difficultySize[Int]` | 150 - 154
`difficulty`          | 154 - (154 + `difficultySize`)
`equihashSolution`    | (154 + `difficultySize`) - `bytes.len`

### Payload

Block payload

ModifierId | 102
-----------|:----
Bytes total| 36 + (4 + `xLen`) * `xQty`

Field name | Bytes distribution
-----------|-------------------
`headerId[ModifierId]` | 0 - 32
`txQty[Int]`           | 32 - 36
`txs[Seq[Transaction]]`| 36 - \[`xLen`: 0 - 4; 4 - `xLen`\] * xQty

### ADProofs

Authenticated dictionary proofs

ModifierId | 104
-----------|:----
Bytes total| 32 + `proofBytes.len`

Field name | Bytes distribution
-----------|-------------------
`headerId[ModifierId]` | 0 - 32
`proofBytes`           | 32 - `bytes.len`


### Transaction

Atomic state modifier

ModifierId | 2
-----------|:----
Bytes total| 20 + (\[`xLen`: 0 - 2; 2 - `xLen`\] * `xQty`) + (\[`yLen`: 0 - 2; 2 - `yLen`\] * `yQty`) + `proof.len`

Field name | Bytes distribution
-----------|-------------------
`fee[Long]`| 0 - 8
`timestamp[Long]`| 8 - 16
`unlockersQty[Short]` | 16 - 18
`directivesQty[Short]`| 18 - 20
`inputs[Seq[Input]]` | 20 - (\[`xLen`: 0 - 2; 2 - `xLen`\] * `xQty`)
`directives[Seq[Directive]]`| 20 + (\[`xLen`: 0 - 2; 2 - `xLen`\] * `xQty`) - 20 + (\[`xLen`: 0 - 2; 2 - `xLen`\] * `xQty`) + (\[`yLen`: 0 - 2; 2 - `yLen`\] * `yQty`)
`proofOpt[Option[Proof]]`  | ? 20 + (\[`xLen`: 0 - 2; 2 - `xLen`\] * `xQty`) + (\[`yLen`: 0 - 2; 2 - `yLen`\] * `yQty`) - `bytes.len`


### EncryProposition

Asset locker

Bytes total | 32
------------|---

Field name | Bytes distribution
-----------|-------------------
`contractHash` | 0 - 32


### Input

Bytes total | arbitrary length
------------|-----------------

Field name | Bytes distribution
-----------|-------------------
`boxId[ModifierId]` | 0 - 32
`contractLen[Short]`| 32 - 34
`contract` (*1) | 34 - `bytes.len`

(1) - Contract is encoded with discriminated codec from `scodec`.

### Proof

Bytes total | arbitrary length
------------|-----------------

Field name | Bytes distribution
-----------|-------------------
`valueLen[Short]`| 0 - 2
`value[BoxedValue]` | 2 - `bytes.len`

### TransaferDirective <- Directive

Bytes total | 45 \|\| 77
------------|-----------

Field name | Bytes distribution
-----------|-------------------
`address[Address]`| 0 - 37
`amount[Long]` | 37 - 45
`tokenIdOpt[Option[Bytes]]`| 45 - ? 77


### DataDirective <- Directive

Bytes total | arbitrary length
------------|-----------------

Field name | Bytes distribution
-----------|-------------------
`contractHash`| 0 - 32
`dataLen[Int]` | 32 - 36
`data`| 36 - `bytes.len`


### AssetBox <- Box

Box holding any monetary asset

TypeId | 1
-----------|:----
Bytes total| 2 + `propositionLen` + 8 + 8 + `x`

Field name | Bytes distribution
-----------|-------------------
`propositionLen[Short]` | 0 - 2
`proposition[EncryProposition]` | 2 - `propositionLen`
`nonce[Long]` | (2 + `propositionLen`) - (2 + `propositionLen` + 8)
`amount[Long]`| (2 + `propositionLen` + 8) - (2 + `propositionLen` + 8 + 8)
`tokenIdOpt[Option[ADKey]]`| ? (2 + `propositionLen` + 8 + 8) - `bytes.len`


### DataBox <- Box

Box holding arbitrary data

TypeId | 4
-----------|:----
Bytes total| 2 + `propositionLen` + 8 + 8 + `x`

Field name | Bytes distribution
-----------|-------------------
`propositionLen[Short]` | 0 - 2
`proposition[EncryProposition]` | 2 - `propositionLen`
`nonce[Long]` | (2 + `propositionLen`) - (2 + `propositionLen` + 8)
`dataLen[Short]`| (2 + `propositionLen` + 8) - (2 + `propositionLen` + 8 + 2)
`data`| (2 + `propositionLen` + 8 + 2) - (2 + `propositionLen` + 8 + 2 + `dataLen`)


## Network messages

Ephemerial messages used for nodes communication (aren't persisted)

Message code | Name | Description | Length
-------------|------|-------------|--------
65           | Sync | Comparison with other node's view | `x` < `MaxBlockIds * ModifierIdSize + 1`
55           | Inv  | Check whether other node has particular modifier | Arbitrary
22           | RequestModifier | Particular modifier request | Arbitrary
33           | Modifier | Response containing modifier | Arbitrary
1            | GetPeers | Other node's nown peers request | Arbitrary
2            | Peers    | Response containing list of peers | Arbitrary

