package scorex.core

package object utils {

  def concatBytes(seq: Traversable[Array[Byte]]): Array[Byte] = {
    val length: Int = seq.map(_.length).sum
    val result: Array[Byte] = new Array[Byte](length)
    var pos: Int = 0
    seq.foreach{ array =>
      System.arraycopy(array, 0, result, pos, array.length)
      pos += array.length
    }
    result
  }

  def concatFixLengthBytes(seq: Traversable[Array[Byte]]): Array[Byte] = seq.headOption match {
    case None       => Array[Byte]()
    case Some(head) => concatFixLengthBytes(seq, head.length)
  }


  def concatFixLengthBytes(seq: Traversable[Array[Byte]], length: Int): Array[Byte] = {
    val result: Array[Byte] = new Array[Byte](seq.toSeq.length * length)
    var index = 0
    seq.foreach { s =>
      Array.copy(s, 0, result, index, length)
      index += length
    }
    result
  }

}
