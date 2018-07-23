# Data structures

Note, for encoding\decoding of collections recursive length prefixing technique is used (example: \[`xLen`: 0-4; 4-`xLen`\], where `xLen ` = `element.bytes`  - means that each `element.bytes` record in collection is prefixed with its encoded length)

ModifierId | Bytes distribution | Bytes total | Desciption
-----------|--------------------|-------------|------------
101        | `version[VersionId]`: 0-1; `parentId[ModifierId]`: 1-33; `adProofsRoot`: 33-65; `stateRoot`: 65 -98; `txsRoot`: 98-130; `timestamp[Long]`: 130-138; `height[Int]`: 138-142; `nonce[Long]`: 142-150; `difficultySize[Int]`: 150-154; `difficulty`: 154-(154+`difficultySize`); `equihashSolution`: (154+`difficultySize`)-bytes.len | 154 + `difficultySize` + `equihashSolutionSize` | Block header
102        | `headerId[ModifierId]`: 0-32; `txQty`: 32-36; `txs`: 36-\[`xLen`: 0-4; 4-`xLen`\] * xQty | 36 + (4 + `xLen`) * `xQty` | Block payload
104        | `headerId[ModifierId]`: 0-32; `proofBytes`: 32-`bytes.len` | 32 + `proofBytes.len` | Authenticated dictionary proofs
2          | `fee[Long]`: 0-8; `timestamp[Long]`: 8-16; `unlockersQty[Short]`: 16-18; `directivesQty[Short]`: 18-20; `inputs[Seq[Input]]`: 20-(\[`xLen`: 0-2; 2-`xLen`\] * `xQty`); `directives[Seq[Directive]]`: 20 + (\[`xLen`: 0-2; 2-`xLen`\] * `xQty`) - 20 + (\[`xLen`: 0-2; 2-`xLen`\] * `xQty`) + (\[`yLen`: 0-2; 2-`yLen`\] * `yQty`); `proofOpt[Option[Proof]]`: ? 20 + (\[`xLen`: 0-2; 2-`xLen`\] * `xQty`) + (\[`yLen`: 0-2; 2-`yLen`\] * `yQty`) - `bytes.len` | 20 + (\[`xLen`: 0-2; 2-`xLen`\] * `xQty`) + (\[`yLen`: 0-2; 2-`yLen`\] * `yQty`) + `proof.len` | Transaction

