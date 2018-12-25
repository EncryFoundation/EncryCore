package encry.storage.levelDb.forksTree

import encry.modifiers.InstanceFactory
import encry.modifiers.state.box.AssetBox
import encry.settings.{Constants, EncryAppSettings}
import encry.utils.{EncryGenerator, FileHelper}
import org.encryfoundation.common.Algos
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{Matchers, PropSpec}

class WalletForksTreeTest extends PropSpec with Matchers with EncryGenerator with InstanceFactory {

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
