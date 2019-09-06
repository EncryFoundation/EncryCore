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

  def nodeName(name: String): Config = ConfigFactory.parseString(
    s"""
       |encry.network.nodeName="$name"
    """.stripMargin
  )

  def connectOnlyWithKnownPeers(connectOnlyWithKnownPeersEnable: Boolean): Config = ConfigFactory.parseString(
    s"""
       |encry.network.connectOnlyWithKnownPeers=$connectOnlyWithKnownPeersEnable
    """.stripMargin
  )

  def miningDelay(miningDelay: Int): Config = ConfigFactory.parseString(
    s"""
       |encry.node.miningDelay=${miningDelay}s
    """.stripMargin
  )

  def knownPeers(peers: Seq[(String, Int)]): Config = ConfigFactory.parseString({
    val peerInfoSeq: Seq[String] = peers.map(n =>
      s"""
         |"${n._1}:${n._2}"
       """.stripMargin)
    val peerInfoStr: String = peerInfoSeq.mkString("[", ",", "]")
    s"""
       |encry.network.knownPeers=$peerInfoStr
     """.stripMargin
  })

  def mnemonicKey(key: String): Config = ConfigFactory.parseString(
    s"""
       |encry.wallet.seed="$key"
     """.stripMargin
  )

  def networkAddress(address: String): Config = ConfigFactory.parseString(
    s"""
       |encry.network.bindAddress = "$address"
     """.stripMargin
  )

  def apiAddress(address: String): Config = ConfigFactory.parseString(
    s"""
       |encry.restApi.bindAddress = "$address"
     """.stripMargin
  )

  def constantsClass(name: String): Config = ConfigFactory.parseString(
    s"""
       |encry.node.constantsClass="$name"
    """.stripMargin
  )

}
