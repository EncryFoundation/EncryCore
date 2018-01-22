package encry.modifiers.history.block.header

case class EncryHeaderChain(headers: Seq[EncryBlockHeader]) {
  headers.indices.foreach { i =>
    if (i > 0) require(headers(i).parentId sameElements headers(i - 1).id)
  }

  def exists(f: EncryBlockHeader => Boolean): Boolean = headers.exists(f)

  def head: EncryBlockHeader = headers.head

  def headOption: Option[EncryBlockHeader] = headers.headOption

  def last: EncryBlockHeader = headers.last

  def tail: EncryHeaderChain = EncryHeaderChain(headers.tail)

  def take(i: Int) = EncryHeaderChain(headers.take(i))

  def takeAfter(h: EncryBlockHeader): EncryHeaderChain = {
    val commonIndex = headers.indexWhere(_.id sameElements h.id)
    val commonBlockThenSuffixes = headers.takeRight(headers.length - commonIndex)
    EncryHeaderChain(commonBlockThenSuffixes)
  }

  def apply(idx: Int): EncryBlockHeader = headers(idx)

  lazy val size: Int = length

  lazy val length: Int = headers.size

  def ++(c: EncryHeaderChain) = EncryHeaderChain(headers ++ c.headers)
}

object EncryHeaderChain {
  lazy val empty = EncryHeaderChain(Seq())
}
