package encry.view.state

import java.io.File

import io.iohk.iodb.{LSMStore, Store}
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.authds.avltree.batch._
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256Unsafe, Digest32}
import scorex.utils.Random

import scala.util.{Failure, Success, Try}

class AVLStorageWithPersistentProverSpec extends PropSpec with Matchers {

  implicit val hf: Blake2b256Unsafe = new Blake2b256Unsafe

  val dir: File = new File(s"${System.getProperty("user.dir")}/test-data/state1")
  dir.mkdir()

  assert(dir.exists() && dir.isDirectory && dir.listFiles.isEmpty, "dir is invalid.")

  val stateStore: Store = new LSMStore(dir, keepVersions = 10)

  // `valueSize` should equals value length of initial modifications.
  // Otherwise `persistentProver.digest` is unpredictable
  // (despite the fact, that `BatchAVLProver.valueLengthOpt = None`)
  private lazy val np =
    NodeParameters(keySize = 32, valueSize = 64, labelSize = 32)

  protected lazy val storage = new VersionedIODBAVLStorage(stateStore, np)

  // Here `BatchAVLProver.valueLengthOpt` is optional, but
  // `VersionedIODBAVLStorage.NodeParameters.valueSize` is required
  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, Blake2b256Unsafe] =
    PersistentBatchAVLProver.create(
      new BatchAVLProver[Digest32, Blake2b256Unsafe](
        keyLength = 32, valueLengthOpt = None), storage).get

  def genProof(mods: Seq[Modification], rollBackTo: ADDigest): Try[(SerializedAdProof, ADDigest)] = {

    def rollback(): Try[Unit] = Try(
      persistentProver.rollback(rollBackTo).ensuring(_.isSuccess && persistentProver.digest.sameElements(rollBackTo))
    ).flatten

    Try {
      if (!(persistentProver.digest.sameElements(rollBackTo) &&
        storage.version.get.sameElements(rollBackTo) &&
        stateStore.lastVersionID.get.data.sameElements(rollBackTo))) Failure(new Error("Bad state version."))

      mods.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, m) =>
        t.flatMap(_ => {
          val opRes = persistentProver.performOneOperation(m)
          opRes
        })
      }.get

      val proof = persistentProver.generateProofAndUpdateStorage()
      val digest = persistentProver.digest

      proof -> digest
    } match {
      case Success(result) => rollback().map(_ => result)
      case Failure(e) => rollback().flatMap(_ => Failure(e))
    }
  }

  def applyModifications(mods: Seq[Modification]): Unit =
    mods.foreach(m => {
      persistentProver.performOneOperation(m).ensuring(_.isSuccess, "Mod application failed.")
    })

  private val initialMods32 = (0 until 100)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(32)(i.toByte)))

  private val initialMods64 = (0 until 100)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(64)(i.toByte)))

  private val initialMods128 = (0 until 100)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(128)(i.toByte)))

  private val mods32 = (0 until 50)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(32)(i.toByte)))

  private val mods64 = (0 until 150)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(64)(i.toByte)))

  private val mods128 = (0 until 50)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(128)(i.toByte)))

  // The result of this test is unstable, even if modifiers values is always of the same size.
  property("Digest (proof) == Digest (actual) after mods application. mods64 at genesis, then mods128") {

    // Setting up initial state.
    applyModifications(initialMods64)

    persistentProver.generateProofAndUpdateStorage()

    lazy val afterGenesisDigest = persistentProver.digest

    val proof = genProof(mods128, afterGenesisDigest)

    // Applying mods with greater values.
    applyModifications(mods128)

    persistentProver.generateProofAndUpdateStorage()

    proof.isSuccess shouldBe true

    proof.get._2.sameElements(persistentProver.digest) shouldBe true

    mods128.forall(m => persistentProver.unauthenticatedLookup(m.key).isDefined) shouldBe true
  }
}
