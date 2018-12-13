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
}
