package encry.storage.levelDb.forksTree

import java.io.File

import encry.modifiers.InstanceFactory
import encry.modifiers.history.Block
import encry.modifiers.state.StateModifierSerializer
import encry.modifiers.state.box.{AssetBox, EncryBaseBox}
import encry.settings.{Constants, EncryAppSettings}
import encry.utils.{EncryGenerator, FileHelper}
import org.encryfoundation.common.Algos
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{Matchers, PropSpec}

class WalletForksTreeTest extends PropSpec with Matchers with EncryGenerator with InstanceFactory {

  property("Adding boxes to cache") {

    val settings = EncryAppSettings.read

    val db: DB = {
      val dir = FileHelper.getRandomTempDir
      if (!dir.exists()) dir.mkdirs()
      if (!dir.isDirectory) throw new IllegalArgumentException(s"Can't find directory at '${settings.directory}/walletLevelDB'")
      LevelDbFactory.factory.open(dir, new Options)
    }

    val walletTree = WalletForksTree(db)

    val boxes = (0 to 100).foldLeft(Seq.empty[EncryBaseBox])( (boxes, _) => boxes :+ genAssetBox(randomAddress))

    walletTree.addBoxesToCache(boxes)

    StateModifierSerializer
      .parseBytes(db.get("cache".getBytes ++ boxes.head.id), boxes.head.id.head).get.id shouldEqual boxes.head.id
  }

  property("Dummy block with 1 coinbase tx applying. Wallet balance should increase") {

    val settings = EncryAppSettings.read

    val fakeBlock = generateGenesisBlock

    fakeBlock.payload.transactions.head.newBoxes.head.asInstanceOf[AssetBox].amount

    val db: DB = {
      val dir = FileHelper.getRandomTempDir
      if (!dir.exists()) dir.mkdirs()
      if (!dir.isDirectory) throw new IllegalArgumentException(s"Can't find directory at '${settings.directory}/walletLevelDB'")
      LevelDbFactory.factory.open(dir, new Options)
    }

    val walletTree = WalletForksTree(db)

    walletTree.add(fakeBlock)

    val boxAbount = fakeBlock.payload.transactions.head.newBoxes.head.asInstanceOf[AssetBox].amount

    walletTree
      .getBalances
      .map(elem => Algos.encode(elem._1) -> elem._2)
      .getOrElse(Algos.encode(Constants.IntrinsicTokenId), 0L) shouldEqual boxAbount
  }

}
