package encry.view.state

import java.io.File

import encry.local.TransactionFactory
import encry.modifiers.state.box.OpenBox
import encry.modifiers.state.box.proposition.HeightProposition
import encry.settings.Constants
import encry.view.history.Height
import io.iohk.iodb.LSMStore
import scorex.crypto.authds.ADValue
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

class UtxoStateTest extends org.scalatest.FunSuite {

  test("BatchAVLProver should have the same digest after rollback as before.") {

    val boxesType1 = TransactionFactory.genAssetBoxes
    val boxesType2 = IndexedSeq(
      OpenBox(HeightProposition(Height @@ 111L), 99L, 1000L),
      OpenBox(HeightProposition(Height @@ 101L), 199L, 1000L)
    )

    val boxHolder = BoxHolder(boxesType1 ++ boxesType2)

    val dir: File = new File("/Encry/test-data")
    assert(dir.exists() && dir.isDirectory, "dir is invalid.")

    val store = new LSMStore(dir, keySize = 32, keepVersions = Constants.keepVersions)

    implicit val hf: Blake2b256Unsafe = new Blake2b256Unsafe

    val prover: BatchAVLProver[Digest32, Blake2b256Unsafe] =
      new BatchAVLProver[Digest32, Blake2b256Unsafe](keyLength = 32, valueLengthOpt = None)

    val np = NodeParameters(keySize = 32, valueSize = 0, labelSize = 32)

    val storage: VersionedAVLStorage[Digest32] = new VersionedIODBAVLStorage(store, np)

    val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
      PersistentBatchAVLProver.create(prover, storage).get

    val initialDigest = persistentProver.digest

    boxHolder.sortedBoxes.foreach(b => prover.performOneOperation(Insert(b.id, ADValue @@ b.bytes))
      .ensuring(_.isSuccess))

    persistentProver.rollback(initialDigest)

    val afterRollbackDigest = persistentProver.digest

    assert(afterRollbackDigest sameElements initialDigest, "Invalid digest after rollback.")
  }

}
