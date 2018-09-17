package encry.modifiers.history.block.header

import encry.modifiers.history.Header

case class HeaderChain(headers: IndexedSeq[Header]) {
  headers.indices.foreach { i =>
    if (i > 0) require(headers(i).parentId sameElements headers(i - 1).id)
  }

  def exists(f: Header => Boolean): Boolean = headers.exists(f)

  def head: Header = headers.head

  def headOption: Option[Header] = headers.headOption

  def last: Header = headers.last

  def tail: HeaderChain = HeaderChain(headers.tail)

  def take(i: Int): HeaderChain = HeaderChain(headers.take(i))

  def takeAfter(h: Header): HeaderChain = {
    val commonIndex = headers.indexWhere(_.id sameElements h.id)
    val commonBlockThenSuffixes = headers.takeRight(headers.length - commonIndex)
    HeaderChain(commonBlockThenSuffixes)
  }

  def apply(idx: Int): Header = headers(idx)

  lazy val size: Int = length

  lazy val length: Int = headers.size

  def ++(c: HeaderChain): HeaderChain = HeaderChain(headers ++ c.headers)
}

object HeaderChain {

  lazy val empty = HeaderChain(IndexedSeq.empty[Header])

  def apply(seq: Seq[Header]): HeaderChain = HeaderChain(seq.toIndexedSeq)
}
