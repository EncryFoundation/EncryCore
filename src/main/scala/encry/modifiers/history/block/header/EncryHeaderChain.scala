package encry.modifiers.history.block.header

case class EncryHeaderChain(headers: IndexedSeq[Header]) {
  headers.indices.foreach { i =>
    if (i > 0) require(headers(i).parentId sameElements headers(i - 1).id)
  }

  def exists(f: Header => Boolean): Boolean = headers.exists(f)

  def head: Header = headers.head

  def headOption: Option[Header] = headers.headOption

  def last: Header = headers.last

  def tail: EncryHeaderChain = EncryHeaderChain(headers.tail)

  def take(i: Int): EncryHeaderChain = EncryHeaderChain(headers.take(i))

  def takeAfter(h: Header): EncryHeaderChain = {
    val commonIndex = headers.indexWhere(_.id sameElements h.id)
    val commonBlockThenSuffixes = headers.takeRight(headers.length - commonIndex)
    EncryHeaderChain(commonBlockThenSuffixes)
  }

  def apply(idx: Int): Header = headers(idx)

  lazy val size: Int = length

  lazy val length: Int = headers.size

  def ++(c: EncryHeaderChain): EncryHeaderChain = EncryHeaderChain(headers ++ c.headers)
}

object EncryHeaderChain {

  lazy val empty = EncryHeaderChain(IndexedSeq.empty[Header])

  def apply(seq: Seq[Header]): EncryHeaderChain = EncryHeaderChain(seq.toIndexedSeq)
}
