package scorex.crypto.authds.avltree.batch

@specialized
case class NodeParameters(keySize: Int, valueSize: Option[Int], labelSize: Int)
