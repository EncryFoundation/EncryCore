package encry.avltree

import com.google.common.primitives.Longs
import com.typesafe.scalalogging.StrictLogging
import encry.avltree
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.encryfoundation.common.utils.TaggedTypes.{ADDigest, ADKey, ADValue}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import encry.avltree.helpers.TestHelper
import encry.settings.EncryAppSettings
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDB
import encry.storage.levelDb.versionalLevelDB.VersionalLevelDBCompanion.VersionalLevelDbKey
import org.encryfoundation.common.Algos
import scorex.crypto.encode.{Base16, Base58}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.utils.{Random => RandomBytes}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Try}

class VersionedAVLStorageSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TestHelper
  with StrictLogging {

  override protected val KL = 32
  override protected val VL = 8
  override protected val LL = 32

  def kvGen: Gen[(ADKey, ADValue)] = for {
    key <- Gen.listOfN(KL, Arbitrary.arbitrary[Byte]).map(_.toArray) suchThat
      (k => !(k sameElements Array.fill(KL)(-1: Byte)) && !(k sameElements Array.fill(KL)(0: Byte)) && k.length == KL)
    value <- Gen.listOfN(VL, Arbitrary.arbitrary[Byte]).map(_.toArray)
  } yield (ADKey @@ key, ADValue @@ value)


  /**
    * List of all test cases
    */

  val rollbackTest: PERSISTENT_PROVER => Unit = { (prover: PERSISTENT_PROVER) =>

    def ops(s: Int, e: Int): Unit = (s until e).foreach { i =>
      prover.performOneOperation(avltree.Insert(ADKey @@ Blake2b256("k" + i).take(KL),
        ADValue @@ Blake2b256("v" + i).take(VL)))
    }

    ops(0, 100)
    prover.generateProofAndUpdateStorage()

    val digest = prover.digest
    val digest16String = Algos.encode(digest)

    ops(100, 200)
    prover.generateProofAndUpdateStorage()

    Algos.encode(prover.digest) should not equal digest16String

    prover.rollback(digest)

    Algos.encode(prover.digest) shouldEqual digest16String

    prover.checkTree(true)
  }

  // Read-only access to some elements in parallel should not affect modifications application
  val parallelReadTest: PERSISTENT_PROVER => Unit = {
    def performModifications(prover: PERSISTENT_PROVER, inserts: Seq[avltree.Insert]) = {
      inserts.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, m) =>
        t.flatMap(_ => prover.performOneOperation(m))
      }.get
      prover.generateProofAndUpdateStorage()
    }

    def startParallelReads(prover: PERSISTENT_PROVER) = {
      Future {
        def loop(): Unit = {
          Thread.sleep(10)
          Try(prover.digest)
          loop()
        }

        loop()
      }
    }

    prover: PERSISTENT_PROVER =>
      val pairs = (1 to 10000).map(_ => kvGen.sample.get)
      pairs.foreach(p => prover.performOneOperation(avltree.Insert(p._1, p._2)))
      prover.generateProofAndUpdateStorage

      val blocks: Seq[Seq[avltree.Insert]] = (0 until 10).map(_ => (0 until 10).flatMap(_ => kvGen.sample)
        .map(kv => avltree.Insert(kv._1, kv._2)))

      val parallelReadsFuture: Future[Unit] = startParallelReads(prover)
      // apply blocks
      blocks.foldLeft(prover.digest) { (startRoot, block) =>
        prover.digest shouldEqual startRoot

        performModifications(prover, block)
        prover.rollback(startRoot)
        Base16.encode(prover.digest) shouldBe Base16.encode(startRoot)

        performModifications(prover, block)

        prover.digest
      }
  }


  // Test similar to blockchain workflow - generate proofs for some modifications, rollback, apply modifications
  val blockchainWorkflowTest: PERSISTENT_PROVER => Unit = { prover: PERSISTENT_PROVER =>
    def metadata(modId: Array[Byte], stateRoot: ADDigest): Seq[(Array[Byte], Array[Byte])] = {
      val idStateDigestIdxElem: (Array[Byte], Array[Byte]) = modId -> stateRoot
      val stateDigestIdIdxElem = Blake2b256(stateRoot) -> modId
      val bestVersion = Blake2b256("best state version") -> modId

      Seq(idStateDigestIdxElem, stateDigestIdIdxElem, bestVersion)
    }

    def intToKey(i: Int, seed: Int = 0): ADKey = ADKey @@ Blake2b256(s"key-$i-$seed").take(KL)

    def intToValue(i: Int, seed: Int = 0): ADValue = ADValue @@ Blake2b256(s"val-$i-$seed").take(VL)

    val initialElementsSize = 10000

    val initialElements = (0 until initialElementsSize).map(i => avltree.Insert(intToKey(i), intToValue(i)))
    initialElements.foreach(op => prover.performOneOperation(op).get)
    prover.generateProofAndUpdateStorage

    val toInsert: Seq[avltree.Insert] = (0 until 1000) map { i =>
      avltree.Insert(intToKey(i, 1), intToValue(i, 1))
    }
    val toRemove: Seq[avltree.Remove] = initialElements.take(1000).map(_.key).map(i => avltree.Remove(i))
    val mods: Seq[Modification] = toInsert ++ toRemove
    val nonMod = prover.avlProver.generateProofForOperations(mods).get

    mods.foldLeft[Try[Option[ADValue]]](Success(None)) { case (t, m) =>
      t.flatMap(_ => {
        prover.performOneOperation(m)
      })
    }.get
    val md = metadata(Blake2b256(nonMod._1 ++ nonMod._2), nonMod._2)
    prover.generateProofAndUpdateStorage(md)

  }

  val basicTest: (PERSISTENT_PROVER, STORAGE) => Unit = { (prover: PERSISTENT_PROVER, storage: STORAGE) =>
    var digest = prover.digest

    def oneMod(aKey: ADKey, aValue: ADValue): Unit = {
      prover.digest shouldBe digest

      val m = avltree.Insert(aKey, aValue)
      prover.performOneOperation(m)
      val pf = prover.generateProofAndUpdateStorage()

      val verifier = createVerifier(digest, pf)
      verifier.performOneOperation(m).isSuccess shouldBe true
      Base58.encode(prover.digest) should not equal Base58.encode(digest)
      Base58.encode(prover.digest) shouldEqual Base58.encode(verifier.digest.get)

      prover.rollback(digest).get

      prover.checkTree(true)

      prover.digest shouldBe digest

      prover.performOneOperation(m)
      val pf2 = prover.generateProofAndUpdateStorage()

      pf shouldBe pf2

      prover.checkTree(true)

      val verifier2 = createVerifier(digest, pf2)
      verifier2.performOneOperation(m).isSuccess shouldBe true

      digest = prover.digest
    }

    (1 to 100).foreach { _ =>
      val (aKey, aValue) = kvGen.sample.get
      oneMod(aKey, aValue)
    }

    val prover2 = createPersistentProver(storage)
    prover2.digest.toBase58 shouldEqual prover.digest.toBase58
    prover2.checkTree(postProof = true)
  }

  val rollbackVersionsTest: (PERSISTENT_PROVER, STORAGE) => Unit = { (prover: PERSISTENT_PROVER, storage: STORAGE) =>
    (0L until 50L).foreach { long =>
      val insert = avltree.Insert(ADKey @@ RandomBytes.randomBytes(32),
        ADValue @@ com.google.common.primitives.Longs.toByteArray(long))
      prover.performOneOperation(insert)
      prover.generateProofAndUpdateStorage()
      prover.digest
    }

    noException should be thrownBy storage.rollbackVersions.foreach(v => prover.rollback(v).get)
  }

  def testAddInfoSaving(createStore: (Int) => VersionalLevelDB): Unit = {
    val store = createStore(1000)
    val settings = EncryAppSettings.read
    val storage = createVersionedStorage(store, settings)
    val prover = createPersistentProver(storage)

    implicit def arrayToWrapper(in: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(in)

    val insert1 = avltree.Insert(ADKey @@ RandomBytes.randomBytes(32), ADValue @@ Longs.toByteArray(1L))
    val insert2 = avltree.Insert(ADKey @@ RandomBytes.randomBytes(32), ADValue @@ Longs.toByteArray(2L))
    val insert3 = avltree.Insert(ADKey @@ RandomBytes.randomBytes(32), ADValue @@ Longs.toByteArray(3L))
    val insert4 = avltree.Insert(ADKey @@ RandomBytes.randomBytes(32), ADValue @@ Longs.toByteArray(4L))
    val insert5 = avltree.Insert(ADKey @@ RandomBytes.randomBytes(32), ADValue @@ Longs.toByteArray(5L))

    val addInfo1 = RandomBytes.randomBytes(32) -> Longs.toByteArray(6L)
    val addInfo2 = RandomBytes.randomBytes(32) -> Longs.toByteArray(7L)

    prover.performOneOperation(insert1)
    prover.generateProofAndUpdateStorage()
    val digest1 = prover.digest

    prover.performOneOperation(insert2)
    prover.performOneOperation(insert3)
    prover.generateProofAndUpdateStorage(Seq(addInfo1))

    val digest2 = prover.digest

    prover.performOneOperation(insert4)
    prover.performOneOperation(insert5)
    prover.generateProofAndUpdateStorage(Seq(addInfo2))


    store.get(VersionalLevelDbKey @@ addInfo1._1) shouldBe defined
    store.get(VersionalLevelDbKey @@ addInfo2._1) shouldBe defined

    storage.rollback(digest2).get

    store.get(VersionalLevelDbKey @@ addInfo1._1) shouldBe defined
    store.get(VersionalLevelDbKey @@ addInfo2._1) shouldBe None

    storage.rollback(digest1).get

    store.get(VersionalLevelDbKey @@ addInfo1._1) shouldBe None
    store.get(VersionalLevelDbKey @@ addInfo2._1) shouldBe None

  }

  def removeFromLargerSetSingleRandomElementTest(createStore: (Int) => VersionalLevelDB): Unit = {
    val minSetSize = 10000
    val maxSetSize = 200000
    val settings = EncryAppSettings.read

    forAll(Gen.choose(minSetSize, maxSetSize), Arbitrary.arbBool.arbitrary) { case (cnt, generateProof) =>
      whenever(cnt > minSetSize) {

        val store = createStore(0)
        val t = Try {
          var keys = IndexedSeq[ADKey]()
          val p = new BatchAVLProver[Digest32, HF](KL, Some(VL))

          (1 to cnt) foreach { _ =>
            val key = ADKey @@ RandomBytes.randomBytes(KL)
            val value = ADValue @@ RandomBytes.randomBytes(VL)

            keys = key +: keys

            p.performOneOperation(avltree.Insert(key, value)).isSuccess shouldBe true
            p.unauthenticatedLookup(key).isDefined shouldBe true
          }

          if (generateProof) p.generateProof()
          val storage = createVersionedStorage(store, settings)
          assert(storage.isEmpty)

          val prover = PersistentBatchAVLProver.create[D, HF](p, storage, paranoidChecks = true).get

          val keyPosition = scala.util.Random.nextInt(keys.length)
          val rndKey = keys(keyPosition)

          prover.unauthenticatedLookup(rndKey).isDefined shouldBe true
          val removalResult = prover.performOneOperation(avltree.Remove(rndKey))
          removalResult.isSuccess shouldBe true

          if (keyPosition > 0) {
            prover.performOneOperation(avltree.Remove(keys.head)).isSuccess shouldBe true
          }

          keys = keys.tail.filterNot(_.sameElements(rndKey))

          val shuffledKeys = scala.util.Random.shuffle(keys)
          shuffledKeys.foreach { k =>
            prover.performOneOperation(avltree.Remove(k)).isSuccess shouldBe true
          }
        }
        store.close()
        t.get
      }
    }
  }


  /**
    * All checks are being made with both underlying storage implementations
    * 1 VLDB
    */

//  property("Persistence AVL batch prover (VLDB backed) - parallel read-write") {
//    val prover = createPersistentProverWithVLDB()
//    parallelReadTest(prover)
//  }


  property("Persistence AVL batch prover (VLDB backed) - blockchain workflow") {
    val prover = createPersistentProverWithVLDB()
    blockchainWorkflowTest(prover)
  }

  property("Persistence AVL batch prover (VLDB backed) - rollback") {
    val prover = createPersistentProverWithVLDB()
    rollbackTest(prover)
  }

  property("Persistence AVL batch prover (VLDB backed) - basic test") {
    val store = createVLDB()
    val settings = EncryAppSettings.read
    val storage = createVersionedStorage(store, settings)
    val prover = createPersistentProver(storage)
    basicTest(prover, storage)
  }

  property("Persistence AVL batch prover (VLDB backed) - rollback version") {
    val store = createVLDB(1000)
    val settings = EncryAppSettings.read
    val storage = createVersionedStorage(store, settings)
    val prover = createPersistentProver(storage)
    rollbackVersionsTest(prover, storage)
  }


  property("Persistence AVL batch prover (VLDB backed) - remove single random element from a large set") {
    removeFromLargerSetSingleRandomElementTest(_ => createVLDB())
  }

  property("Persistence AVL batch prover (VLDB backed) - save additional info") {
    testAddInfoSaving(createVLDB _)
  }
}
