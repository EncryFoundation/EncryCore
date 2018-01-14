package encry.view.state

import java.io.File

import encry.settings.Constants
import io.iohk.iodb.LSMStore
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADKey, ADValue}
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}

class UtxoStateTest extends org.scalatest.FunSuite {

  test("BatchAVLProver should have the same digest after rollback as before.") {

    val valuesToInsert = (1 until 100).map { i =>
      Array.fill(32)(i.toByte) -> Array.fill(64)(i.toByte)
    }

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

    valuesToInsert.foreach(v => prover.performOneOperation(Insert(ADKey @@ v._1, ADValue @@ v._2))
      .ensuring(_.isSuccess))

    persistentProver.rollback(initialDigest)

    val afterRollbackDigest = persistentProver.digest

    assert(afterRollbackDigest sameElements initialDigest, "Invalid digest after rollback.")
  }

}
