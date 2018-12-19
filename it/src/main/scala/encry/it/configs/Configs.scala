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

  def miningDelay(miningDelay: Int): Config = ConfigFactory.parseString(
    s"""
       |encry.node.miningDelay=${miningDelay}s
    """.stripMargin
  )

  def knownPeers(peers: Seq[String]): Config = ConfigFactory.parseString({
    val port = 9001
    val peerInfoSeq: Seq[String] = peers.map(n =>
      s"""
         |"$n:$port"
       """.stripMargin)
    val peerInfoStr: String = peerInfoSeq.mkString("[", ",", "]")
    println(s"PeerInfo = ${s"""
                              |encry.network.knownPeers=$peerInfoStr
     """.stripMargin}")
    s"""
       |encry.network.knownPeers=$peerInfoStr
     """.stripMargin
  })
}
