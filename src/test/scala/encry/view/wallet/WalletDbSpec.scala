package encry.view.wallet

import com.typesafe.scalalogging.StrictLogging
import encry.modifiers.InstanceFactory
import encry.settings.{ LevelDBSettings, Settings }
import encry.storage.levelDb.versionalLevelDB.{ LevelDbFactory, VersionalLevelDB, VersionalLevelDBCompanion }
import encry.utils.{ EncryGenerator, FileHelper }
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.state.box._
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.{ ADKey, ModifierId }
import org.iq80.leveldb.{ DB, Options }
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{ Matchers, WordSpecLike }
import scorex.utils.Random

class WalletDbSpec
    extends WordSpecLike
    with Matchers
    with InstanceFactory
    with EncryGenerator
    with StrictLogging
    with Settings
    with MockitoSugar {

  def initTestState: (WalletDB, Seq[EncryBaseBox]) = {
    val levelDB: DB                       = LevelDbFactory.factory.open(FileHelper.getRandomTempDir, new Options)
    val vlDB: VersionalLevelDB            = VersionalLevelDBCompanion(levelDB, LevelDBSettings(5))
    def dbInstance: WalletDB              = WalletDB.apply(vlDB, settings)
    val boxesToInsert: List[EncryBaseBox] = genValidPaymentTxs(3).flatMap(_.newBoxes).toList
    dbInstance.updateWallet(
      ModifierId @@ Random.randomBytes(),
      boxesToInsert,
      List.empty,
      settings.constants.IntrinsicTokenId
    )
    (dbInstance, boxesToInsert)
  }

  "WalletDb.getBoxById" should {
    "return non empty value for existed box in db" in {
      val (walletDb: WalletDB, inserted: Seq[EncryBaseBox]) = initTestState
      val comparisonResult: Boolean = inserted.forall { box =>
        val boxFromDB: Option[EncryBaseBox] = walletDb.getBoxById(box.id)
        boxFromDB.isDefined && boxFromDB.forall { takenBox =>
          box.bytes.sameElements(takenBox.bytes)
        }
      }
      comparisonResult shouldBe true

      val boxesToInsert: List[EncryBaseBox] = genValidPaymentTxs(5).flatMap(_.newBoxes).toList
      walletDb.updateWallet(
        ModifierId @@ Random.randomBytes(),
        boxesToInsert,
        inserted.toList,
        settings.constants.IntrinsicTokenId
      )
      val comparisonResultNegative: Boolean = inserted.forall { box =>
        walletDb.getBoxById(box.id).isEmpty
      }
      val comparisonResultPositive: Boolean = boxesToInsert.forall { box =>
        val boxFromDB: Option[EncryBaseBox] = walletDb.getBoxById(box.id)
        boxFromDB.isDefined && boxFromDB.forall { takenBox =>
          box.bytes.sameElements(takenBox.bytes)
        }
      }
      (comparisonResultNegative && comparisonResultPositive) shouldBe true
    }
    "return empty value for non existed box in db" in {
      val (walletDb: WalletDBImpl, _) = initTestState
      walletDb.getBoxById(ADKey @@ Random.randomBytes()).isEmpty shouldBe true
    }
  }

  "WalletDb.getAllWallets" should {
    "return all inserted wallets" in {
      val (walletDb: WalletDBImpl, inserted: Seq[EncryBaseBox]) = initTestState
      val wallets                                               = walletDb.getAllWallets.map(Algos.encode)
      val neededWallets = inserted
        .map(l => Algos.encode(l.proposition.contractHash))
        .toSet
      (wallets.size == neededWallets.size && wallets.forall(neededWallets.contains)) shouldBe true

      val boxesToInsert: List[EncryBaseBox] = genValidPaymentTxs(5).flatMap(_.newBoxes).toList
      walletDb.updateWallet(
        ModifierId @@ Random.randomBytes(),
        boxesToInsert,
        inserted.toList,
        settings.constants.IntrinsicTokenId
      )

      val walletsNew = walletDb.getAllWallets.map(Algos.encode)
      val neededWalletsNew = inserted.map(l => Algos.encode(l.proposition.contractHash)) ++ boxesToInsert.map(
        l => Algos.encode(l.proposition.contractHash)
      )
      (walletsNew.size == neededWalletsNew.size && walletsNew.forall(neededWalletsNew.contains)) shouldBe true
    }
  }

  "WalletDb.getBalances" should {
    "return a correct result" in {}
  }

  "WalletDb.getAssetBoxesByPredicate" should {
    "return a correct result if the result satisfies the predicate" in {
      val (walletDb: WalletDBImpl, _) = initTestState
      val moreBoxes: IndexedSeq[AssetBox] = {
        val key: PrivateKey25519 = genPrivKeys(1).head
        IndexedSeq(
          AssetBox(EncryProposition.addressLocked(key.publicImage.address.address), 11, 999, None),
          AssetBox(EncryProposition.addressLocked(key.publicImage.address.address), 111, 9999, None),
          AssetBox(EncryProposition.addressLocked(key.publicImage.address.address), 112, 9991, None),
          AssetBox(EncryProposition.addressLocked(key.publicImage.address.address), 113, 999654, None),
        )
      }
      walletDb.updateWallet(
        ModifierId @@ Random.randomBytes(),
        moreBoxes.toList,
        List.empty,
        settings.constants.IntrinsicTokenId
      )
      val boxes = walletDb.getAssetBoxesByPredicate(
        moreBoxes.head.proposition.contractHash,
        list => list.map(_.amount).sum == moreBoxes.map(_.amount).sum
      )
      boxes.nonEmpty && boxes.forall { box =>
        moreBoxes.exists(_.bytes sameElements box.bytes)
      } shouldBe true

      walletDb
        .getAssetBoxesByPredicate(
          moreBoxes.head.proposition.contractHash,
          list => list.map(_.amount).sum > moreBoxes.map(_.amount).sum
        )
        .isEmpty shouldBe true

      walletDb.updateWallet(
        ModifierId @@ Random.randomBytes(),
        List.empty,
        moreBoxes.take(2).toList,
        settings.constants.IntrinsicTokenId
      )

      val boxesNew = walletDb.getAssetBoxesByPredicate(
        moreBoxes.head.proposition.contractHash,
        list => list.map(_.amount).sum == moreBoxes.map(_.amount).sum
      )
      boxesNew.nonEmpty && boxesNew.forall { box =>
        moreBoxes.exists(_.bytes sameElements box.bytes)
      } shouldBe false

      val boxesNew1 = walletDb.getAssetBoxesByPredicate(
        moreBoxes.head.proposition.contractHash,
        list => list.map(_.amount).sum == moreBoxes.drop(2).map(_.amount).sum
      )
      boxesNew1.nonEmpty && boxesNew1.forall { box =>
        moreBoxes.drop(2).exists(_.bytes sameElements box.bytes)
      } shouldBe true
    }
  }

  "WalletDb.getDataBoxes" should {
    "return a correct result if the result satisfies the predicate" in {
      val (walletDb: WalletDBImpl, _) = initTestState
      val dataBoxesToInsert: IndexedSeq[DataBox] = {
        val key: PrivateKey25519 = genPrivKeys(1).head
        IndexedSeq(
          DataBox(EncryProposition.addressLocked(key.publicImage.address.address), 99, Random.randomBytes()),
          DataBox(EncryProposition.addressLocked(key.publicImage.address.address), 991, Random.randomBytes()),
          DataBox(EncryProposition.addressLocked(key.publicImage.address.address), 992, Random.randomBytes())
        )
      }
      walletDb.updateWallet(
        ModifierId @@ Random.randomBytes(),
        dataBoxesToInsert.toList,
        List.empty,
        settings.constants.IntrinsicTokenId
      )

      walletDb
        .getDataBoxesByPredicate(
          dataBoxesToInsert.head.proposition.contractHash,
          boxes => boxes.size == dataBoxesToInsert.size
        )
        .nonEmpty shouldBe true

      walletDb
        .getDataBoxesByPredicate(
          dataBoxesToInsert.head.proposition.contractHash,
          boxes => boxes.size > dataBoxesToInsert.size
        )
        .nonEmpty shouldBe false
    }
  }

  "WalletDb.getBalances" should {
    "return correct balances" in {
      val (walletDb: WalletDBImpl, _) = initTestState
      val tkId1                       = Random.randomBytes()
      val tkId2                       = Random.randomBytes()
      val key                         = genPrivKeys(1)
      val key2                        = genPrivKeys(1)
      val boxesToInsertForPerson1: IndexedSeq[EncryBox[EncryProposition]] = {
        IndexedSeq(
          TokenIssuingBox(EncryProposition.addressLocked(key.head.publicImage.address.address), 1234L, 340, tkId1),
          TokenIssuingBox(EncryProposition.addressLocked(key.head.publicImage.address.address), 4321L, 570, tkId1),
          DataBox(EncryProposition.addressLocked(key.head.publicImage.address.address), 99, Random.randomBytes()),
          AssetBox(EncryProposition.addressLocked(key.head.publicImage.address.address), 11, 2000, None),
          AssetBox(EncryProposition.addressLocked(key.head.publicImage.address.address), 111, 3000, None)
        )
      }
      val boxesToInsertForPerson2: IndexedSeq[EncryBox[EncryProposition]] = {
        IndexedSeq(
          TokenIssuingBox(EncryProposition.addressLocked(key2.head.publicImage.address.address), 1234L, 999, tkId2),
          TokenIssuingBox(EncryProposition.addressLocked(key2.head.publicImage.address.address), 4321L, 9991, tkId2),
          DataBox(EncryProposition.addressLocked(key2.head.publicImage.address.address), 99, Random.randomBytes()),
          AssetBox(EncryProposition.addressLocked(key2.head.publicImage.address.address), 11, 999, None),
          AssetBox(EncryProposition.addressLocked(key2.head.publicImage.address.address), 111, 9999, None)
        )
      }
      val ch1 = boxesToInsertForPerson1.head.proposition.contractHash
      val ch2 = boxesToInsertForPerson2.head.proposition.contractHash
      walletDb.updateWallet(
        ModifierId @@ Random.randomBytes(),
        boxesToInsertForPerson1.toList ::: boxesToInsertForPerson2.toList,
        List.empty,
        settings.constants.IntrinsicTokenId
      )

      boxesToInsertForPerson1.take(2).map(x => x.asInstanceOf[TokenIssuingBox]).map(x => x.amount).sum shouldEqual
        walletDb
          .getBalancesByContractHash(ch1)
          .filter { case (id, _) => Algos.encode(id) != Algos.encode(settings.constants.IntrinsicTokenId) }
          .values
          .toList
          .sum

      boxesToInsertForPerson2.take(2).map(x => x.asInstanceOf[TokenIssuingBox]).map(x => x.amount).sum shouldEqual
        walletDb
          .getBalancesByContractHash(ch2)
          .filter { case (id, _) => Algos.encode(id) != Algos.encode(settings.constants.IntrinsicTokenId) }
          .values
          .toList
          .sum

      boxesToInsertForPerson1.drop(3).map(x => x.asInstanceOf[AssetBox]).map(x => x.amount).sum shouldEqual
        walletDb
          .getBalancesByContractHash(ch1)
          .filter { case (id, _) => Algos.encode(id) == Algos.encode(settings.constants.IntrinsicTokenId) }
          .values
          .toList
          .sum

      boxesToInsertForPerson2.drop(3).map(x => x.asInstanceOf[AssetBox]).map(x => x.amount).sum shouldEqual
        walletDb
          .getBalancesByContractHash(ch2)
          .filter { case (id, _) => Algos.encode(id) == Algos.encode(settings.constants.IntrinsicTokenId) }
          .values
          .toList
          .sum

      val boxesToRemoveForPerson1 = IndexedSeq(
        TokenIssuingBox(EncryProposition.addressLocked(key.head.publicImage.address.address), 1234L, 900, tkId1),
        AssetBox(EncryProposition.addressLocked(key.head.publicImage.address.address), 111, 4000, None)
      )

      val boxesToRemoveForPerson2 = IndexedSeq(
        TokenIssuingBox(EncryProposition.addressLocked(key2.head.publicImage.address.address), 1234L, 900, tkId2),
        AssetBox(EncryProposition.addressLocked(key2.head.publicImage.address.address), 111, 4000, None)
      )

      walletDb.updateWallet(
        ModifierId @@ Random.randomBytes(),
        List.empty,
        boxesToRemoveForPerson1.toList ++ boxesToRemoveForPerson2.toList,
        settings.constants.IntrinsicTokenId
      )

      boxesToInsertForPerson1.take(2).map(x => x.asInstanceOf[TokenIssuingBox]).map(x => x.amount).sum -
        boxesToRemoveForPerson1.take(1).map(x => x.asInstanceOf[TokenIssuingBox]).map(x => x.amount).sum shouldEqual
        walletDb
          .getBalancesByContractHash(ch1)
          .filter { case (id, _) => Algos.encode(id) != Algos.encode(settings.constants.IntrinsicTokenId) }
          .values
          .toList
          .sum

      boxesToInsertForPerson2.take(2).map(x => x.asInstanceOf[TokenIssuingBox]).map(x => x.amount).sum -
        boxesToRemoveForPerson2.take(1).map(x => x.asInstanceOf[TokenIssuingBox]).map(x => x.amount).sum shouldEqual
        walletDb
          .getBalancesByContractHash(ch2)
          .filter { case (id, _) => Algos.encode(id) != Algos.encode(settings.constants.IntrinsicTokenId) }
          .values
          .toList
          .sum
    }
  }

  "WalletDb.getTokenIssuingBoxes" should {
    "return a correct result if the result satisfies the predicate" in {
      val (walletDb: WalletDBImpl, _) = initTestState
      val tid1                        = Random.randomBytes()
      val tid2                        = Random.randomBytes()
      val tokenBoxesToInsert: IndexedSeq[TokenIssuingBox] = {
        val key: PrivateKey25519 = genPrivKeys(1).head
        IndexedSeq(
          TokenIssuingBox(EncryProposition.addressLocked(key.publicImage.address.address), 1234L, 999, tid1),
          TokenIssuingBox(EncryProposition.addressLocked(key.publicImage.address.address), 4321L, 9991, tid1),
          TokenIssuingBox(EncryProposition.addressLocked(key.publicImage.address.address), 9887L, 9992, tid2),
          TokenIssuingBox(EncryProposition.addressLocked(key.publicImage.address.address), 768594L, 9993, tid2)
        )
      }
      walletDb.updateWallet(
        ModifierId @@ Random.randomBytes(),
        tokenBoxesToInsert.toList,
        List.empty,
        settings.constants.IntrinsicTokenId
      )

      walletDb
        .getTokenIssuingBoxesByPredicate(
          tokenBoxesToInsert.head.proposition.contractHash,
          boxes => boxes.size == tokenBoxesToInsert.size
        )
        .nonEmpty shouldBe true

      walletDb
        .getDataBoxesByPredicate(
          tokenBoxesToInsert.head.proposition.contractHash,
          boxes => boxes.size > tokenBoxesToInsert.size
        )
        .nonEmpty shouldBe false
    }
  }
}
