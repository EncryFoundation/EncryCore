package encry.it.docker

import com.typesafe.config.ConfigFactory.{defaultApplication, defaultReference}
import encry.modifiers.history.Block
import net.ceedubs.ficus.Ficus._

object BaseTargetChecker {
  def main(args: Array[String]): Unit = {
    val sharedConfig = Docker.genesisOverride
      .withFallback(Docker.configTemplate)
      .withFallback(defaultApplication())
      .withFallback(defaultReference())
      .resolve()
    val settings     = WavesSettings.fromConfig(sharedConfig)
    val genesisBlock = Block.genesis(settings.blockchainSettings.genesisSettings).explicitGet()
    val db           = openeDB ("/tmp/tmp-db")
    val time         = new NTP("ntp.pool.org")
    val bu           = StorageFactory(settings, db, time)
    bu.processBlock(genesisBlock)

    try {
      NodeConfigs.Default.map(_.withFallback(sharedConfig)).collect {
        case cfg if cfg.as[Boolean]("waves.miner.enable") =>
          val account   = PublicKeyAccount(cfg.as[ByteStr]("public-key").arr)
          val address   = account.toAddress
          val balance   = bu.balance(address, None)
          val consensus = genesisBlock.consensusData
          val timeDelay = pos
            .getValidBlockDelay(bu.height, account.publicKey, consensus.baseTarget, balance)
            .explicitGet()

          f"$address: ${timeDelay * 1e-3}%10.3f s"
      }
    } finally time.close()
  }
}
