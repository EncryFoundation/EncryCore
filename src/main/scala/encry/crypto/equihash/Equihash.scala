package encry.crypto.equihash

import java.math.BigInteger
import java.nio.{ByteBuffer, ByteOrder}
import org.apache.commons.lang.ArrayUtils
import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.encryfoundation.common.crypto.equihash.EquihashSolution
import scala.collection.mutable.ArrayBuffer

object Equihash {

  private val wordSize: Int = 32
  private val byteSize: Int = 8

  def nonceToLeBytes(nonce: BigInt): Array[Byte] = {
    val res = for (i <- 0 to 7) yield littleEndianIntToByteArray((nonce >> 32 * i).intValue())
    res.fold(Array.emptyByteArray) { case (acc, nextArr) => ArrayUtils.addAll(acc, nextArr) }
  }


  def hashNonce[T <: Digest](digest: T, nonce: BigInt): Unit = {
    val arr: Array[Byte] = nonceToLeBytes(nonce)
    digest.update(arr, 0, arr.length)
  }

  //todo: takes a lot of time
  def hashSolution[T <: Digest](digest: T, solution: EquihashSolution): Unit = solution.ints map {
    hashXi(digest, _)
  }


  def hashXi[T <: Digest](digest: T, xi: Int): Unit = {
    val arr: Array[Byte] = littleEndianIntToByteArray(xi)
    digest.update(arr, 0, arr.length)
  }

  def countLeadingZeroes(bytes: Array[Byte]): Int = (0 until byteSize * bytes.length).foldLeft(0.toByte) {
    case (res, i) if (bytes(i / byteSize) << i % byteSize & 0x80) == 0 => (res + 1).toByte
    case (res, _) => res
  }

  def hasCollision(ha: Array[Byte], hb: Array[Byte], i: Int, lenght: Int): Boolean =
    ((i - 1) * lenght / 8 until i * lenght / 8).forall(j => ha(j) == hb(j))

  def distinctIndices(a: Seq[Int], b: Seq[Int]): Boolean = !a.exists(v => b.contains(v))

  def xor(ha: Array[Byte], hb: Array[Byte]): Array[Byte] = for {(a, b) <- ha.zip(hb)} yield (a ^ b).toByte

  def expandArray(inp: Array[Byte], outLen: Int, bitLen: Int, bytePad: Int = 0): Array[Byte] = {
    assert(bitLen >= 8 && wordSize >= 7 + bitLen)
    val outWidth: Int = (bitLen + 7) / 8 + bytePad
    assert(outLen == 8 * outWidth * inp.length / bitLen)
    val out: Array[Byte] = new Array[Byte](outLen)
    var accValue: BigInteger = BigInteger.ZERO
    inp.indices.foldLeft(0, 0) {
      case ((accBits, j), i) =>
        accValue = accValue.shiftLeft(8).and(BigInteger.ONE.shiftLeft(wordSize).subtract(BigInteger.ONE))
          .or(BigInteger.valueOf((inp(i) & 0xFF).toLong))
        val possibleAccBits: Int = accBits + 8
        if (possibleAccBits >= bitLen) {
          val correctAccBits: Int = possibleAccBits - bitLen
          (bytePad until outWidth).foreach(x =>
            out.update(j + x, {
              accValue.shiftRight(correctAccBits + (8 * (outWidth - x - 1)))
                .and(BigInteger.valueOf((1 << bitLen) - 1).shiftRight(8 * (outWidth - x - 1))
                  .and(BigInteger.valueOf(0xFF)))
                .byteValue()
            })
          )
          (correctAccBits, j + outWidth)
        } else (possibleAccBits, j)
    }
    out
  }

  def gbpBasic(digest: Blake2bDigest, n: Char, k: Char): Seq[EquihashSolution] = {
    val collisionLength: Int = n / (k + 1)
    val hashLength: Int = (k + 1) * ((collisionLength + 7) / 8)
    val indicesPerHashOutput: Int = 512 / n
    val tmpHash: Array[Byte] = new Array[Byte](digest.getDigestSize)
    var x = for {i <- (0 until Math.pow(2, collisionLength + 1).toInt).toVector} yield {
      val r: Int = i % indicesPerHashOutput
      if (r == 0) {
        val currDigest: Blake2bDigest = new Blake2bDigest(digest)
        hashXi(currDigest, i / indicesPerHashOutput)
        currDigest.doFinal(tmpHash, 0)
      }
      val d: Array[Byte] = tmpHash.slice(r * n / 8, (r + 1) * n / 8)
      val expanded: Array[Byte] = expandArray(d, hashLength, collisionLength)
      expanded -> Seq(i)
    }
    for (i <- 1 until k) {
      x = x.sortBy(_._1.toIterable)
      var xC: Vector[(Array[Byte], Seq[Int])] = Vector.empty[(Array[Byte], Seq[Int])]
      while (x.nonEmpty) {
        val xSize: Int = x.size
        val j: Int = (1 until xSize).find(j => !hasCollision(x.last._1, x(xSize - 1 - j)._1, i, collisionLength))
          .getOrElse(xSize)
        for {
          l <- 0 until j - 1
          m <- l + 1 until j
        } {
          val X1l: (Array[Byte], Seq[Int]) = x(xSize - 1 - l)
          val X1m: (Array[Byte], Seq[Int]) = x(xSize - 1 - m)
          if (distinctIndices(X1l._2, X1m._2)) {
            val concat: Seq[Int] = if (X1l._2.head < X1m._2.head) X1l._2 ++ X1m._2 else X1m._2 ++ X1l._2
            xC = xC :+ (xor(X1l._1, X1m._1) -> concat)
          }
        }
        x = x.dropRight(j)
      }
      x = xC
    }
    x = x.sortBy(_._1.toIterable)
    var solns: Vector[EquihashSolution] = Vector.empty[EquihashSolution]
    while (x.nonEmpty) {
      val XSize: Int = x.length
      val j: Int = (1 until XSize).find(j => !(hasCollision(x.last._1, x(XSize - 1 - j)._1, k, collisionLength) &&
        hasCollision(x.last._1, x(XSize - 1 - j)._1, k + 1, collisionLength))).getOrElse(XSize)
      for {
        l <- 0 until j - 1
        m <- l + 1 until j
      } {
        val res: Array[Byte] = xor(x(XSize - 1 - l)._1, x(XSize - 1 - m)._1)
        if (countLeadingZeroes(res) == 8 * hashLength && distinctIndices(x(XSize - 1 - l)._2, x(XSize - 1 - m)._2)) {
          val p: Seq[Int] =
            if (x(XSize - 1 - l)._2.head < x(XSize - 1 - m)._2.head) x(XSize - 1 - l)._2 ++ x(XSize - 1 - m)._2
            else x(XSize - 1 - m)._2 ++ x(XSize - 1 - l)._2
          solns = solns :+ EquihashSolution(p)
        }
      }
      x = x.dropRight(j)
    }
    solns
  }

  /**
    * Generate n-bit word at specified index.
    * https://github.com/str4d/zcash-pow/blob/master/test-pow.py
    *
    * @param n                Word length in bits
    * @param digestWithoutIdx digest without index
    * @param idx              word index
    * @return word
    */
  def generateWord(n: Char, digestWithoutIdx: Blake2bDigest, idx: Int): BigInteger = {
    val bytesPerWord: Int = n / 8
    val wordsPerHash: Int = 512 / n
    val hidx: Int = idx / wordsPerHash
    val hrem: Int = idx % wordsPerHash
    val idxdata: Array[Byte] = littleEndianIntToByteArray(hidx)
    val ctx1: Blake2bDigest = new Blake2bDigest(digestWithoutIdx)
    ctx1.update(idxdata, 0, idxdata.length)
    val digest: Array[Byte] = new Array[Byte](ctx1.getDigestSize)
    ctx1.doFinal(digest, 0)
    (hrem * bytesPerWord until hrem * bytesPerWord + bytesPerWord).foldLeft(BigInteger.ZERO) {
      case (w, i) => w.shiftLeft(8).or(BigInteger.valueOf((digest(i) & 0xFF).toLong))
    }
  }

  /**
    * Validate an Equihash solution.
    * https://github.com/jorisvr/equihash-xenon/blob/87c5ec80b0817823ef163ef9802ca514dbfa2313/python/validate.py
    *
    * @param n               Word length in bits
    * @param k               2-log of number of indices per solution
    * @param personal        Personal bytes for digest
    * @param header          Block header with nonce
    * @param solutionIndices Solution indices
    * @return Either.right(true) if solution is valid, Either.left("error message") if not.
    */
  def validateSolution(n: Char,
                       k: Char,
                       personal: Array[Byte],
                       header: Array[Byte],
                       solutionIndices: IndexedSeq[Int]): Either[String, Boolean] = for {
    _ <- Either.cond(n > 1, (), s"Incorrect n > 1 parameter: ${n > 1}")
    _ <- Either.cond(k >= 3, (), s"Incorrect k >= 3 parameter: ${k >= 3}")
    _ <- Either.cond(n % 8 == 0, (), s"Incorrect n % 8 == 0 parameter: ${n % 8 == 0}")
    _ <- Either.cond(n % (k + 1) == 0, (), s"Incorrect n % (k + 1) == 0 parameter: ${n % (k + 1) == 0}")
    solutionLength: Int = Math.pow(2, k).toInt
    _ <- Either.cond(solutionIndices.size == solutionLength, (), s"Incorrect solution length: ${solutionIndices.size}")
    _ <- Either.cond(solutionIndices.toSet.size == solutionIndices.size, (), "Duplicate solutions")
    _ <- Either.cond({
      val bytesPerWord: Int = n / 8
      val wordsPerHash: Int = 512 / n
      val outLen: Int = wordsPerHash * bytesPerWord
      val digest: Blake2bDigest = new Blake2bDigest(null, outLen, null, personal)
      digest.update(header, 0, header.length)
      val pairWiseCheck: Boolean = (0 until k).forall(s => {
        val d: Int = 1 << s
        (0 until solutionLength by 2 * d)
          .forall(i => if (solutionIndices(i) >= solutionIndices(i + d)) false else true)
      })
      val words: ArrayBuffer[BigInteger] = (0 until solutionLength).foldLeft(ArrayBuffer.empty[BigInteger]) {
        case (buffer, i) => buffer += generateWord(n, digest, solutionIndices(i))
      }
      val xorConditionsCheck: Boolean = {
        val bitsPerStage: Int = n / (k + 1)
        (0 until k).forall(s => {
          val d: Int = 1 << s
          (0 until solutionLength by 2 * d).forall(i => {
            val w: BigInteger = words(i).xor(words(i + d))
            if (w.shiftRight(n - (s + 1) * bitsPerStage) != BigInteger.ZERO) false
            else {
              words(i) = w
              true
            }
          })
        })
      }
      words.head == BigInteger.ZERO && pairWiseCheck && xorConditionsCheck
    }, (), "Incorrect condition: words.head == BigInteger.ZERO && pairWiseCheck && xorConditionsCheck")
  } yield true

  def littleEndianIntToByteArray(i: Int): Array[Byte] = {
    val bb = ByteBuffer.allocate(Integer.SIZE / 8)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(i)
    bb.array
  }
}