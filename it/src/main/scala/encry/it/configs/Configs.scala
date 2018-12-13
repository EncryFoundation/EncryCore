package encry.it.configs

import com.typesafe.config.{Config, ConfigFactory}

object Configs {

  def mining(miningEnable: Boolean): Config = ConfigFactory.parseString(
    s"""
       |encry.node.mining=$miningEnable
    """.stripMargin
  )

  def offlineGeneration(offlineGeneration: Boolean): Config = ConfigFactory.parseString(
    s"""
       |encry.node.offlineGeneration=$offlineGeneration
    """.stripMargin
  )

  def miningDelay(miningDelay: Int): Config = ConfigFactory.parseString(
    s"""
       |encry.node.miningDelay=${miningDelay}s
    """.stripMargin
  )

  def knownPeers(peers: List[String]): Config = ConfigFactory.parseString(
    s"""
       |encry.network.knownPeers=[${peers.map(peer => s""""$peer"""").mkString(",")}]
    """.stripMargin
  )
}
