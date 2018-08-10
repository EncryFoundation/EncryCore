package scorex.crypto.authds.avltree.batch

import io.iohk.iodb.LSMStore
import org.encryfoundation.common.Algos
import org.scalatest.{Matchers, PropSpec}
import scorex.crypto.authds.avltree.batch.benchmark.IODBBenchmark.getRandomTempDir
import scorex.crypto.authds.{ADDigest, ADKey, ADValue, SerializedAdProof}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.utils.Random
import scala.util.{Failure, Try}

class AVLStorageWithPersistentProverSpec extends PropSpec with Matchers {

  type HF = Blake2b256.type
  implicit val hf: HF = Blake2b256

  private lazy val np: NodeParameters = NodeParameters(keySize = 32, valueSize = None, labelSize = 32)

  protected lazy val storage: VersionedIODBAVLStorage[Digest32] = new VersionedIODBAVLStorage(new LSMStore(getRandomTempDir), np)

  protected lazy val persistentProver: PersistentBatchAVLProver[Digest32, HF] =
    PersistentBatchAVLProver.create(
      new BatchAVLProver[Digest32, HF](
        keyLength = 32, valueLengthOpt = None), storage).get

  def getProof(mods: Seq[Modification]): Try[(SerializedAdProof, ADDigest)] = {
    val rootHash: ADDigest = persistentProver.digest
    if (mods.isEmpty) Failure(new Exception("Got empty modification sequence"))
    else if (!storage.version.exists(_.sameElements(rootHash)))
      Failure(new Exception(s"Invalid storage version: ${storage.version.map(Algos.encode)} != ${Algos.encode(rootHash)}"))
    else persistentProver.avlProver.generateProofForOperations(mods)
  }

  def applyModifications(mods: Seq[Modification]): Unit =
    mods.foreach(m => {
      persistentProver.performOneOperation(m).ensuring(_.isSuccess, "Mod application failed.")
    })

  private val iMods32 = (0 until 100)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(32)(i.toByte)))

  private val iMods64 = (0 until 100)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(64)(i.toByte)))

  private val iMods128 = (0 until 100)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(128)(i.toByte)))

  private val mods32 = (0 until 50)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(32)(i.toByte)))

  private val mods64 = (0 until 150)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(64)(i.toByte)))

  private val mods128 = (0 until 50)
    .map(i => Insert(ADKey @@ Random.randomBytes(), ADValue @@ Array.fill(128)(i.toByte)))

  property("Proof Digest should equal Actual Digest after mods application. 32-bytes values at genesis, then 128") {

    // Setting up initial state.
    applyModifications(iMods32)

    persistentProver.generateProofAndUpdateStorage()

    val proof = getProof(mods128)

    // Applying mods with greater values.
    applyModifications(mods128)

    persistentProver.generateProofAndUpdateStorage()

    proof.isSuccess shouldBe true

    proof.get._2.sameElements(persistentProver.digest) shouldBe true

    mods128.forall(m => persistentProver.unauthenticatedLookup(m.key).isDefined) shouldBe true
  }

  property("Modifiers should be found in storage after sequential application of modifiers of variable size.") {

    applyModifications(iMods128)

    persistentProver.generateProofAndUpdateStorage()

    applyModifications(mods64)

    applyModifications(mods32)

    persistentProver.generateProofAndUpdateStorage()

    iMods128.forall(m => persistentProver.unauthenticatedLookup(m.key).isDefined) shouldBe true

    mods64.forall(m => persistentProver.unauthenticatedLookup(m.key).isDefined) shouldBe true

    mods32.forall(m => persistentProver.unauthenticatedLookup(m.key).isDefined) shouldBe true
  }
}
