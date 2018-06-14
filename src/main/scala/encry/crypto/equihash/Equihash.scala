package encry.crypto.equihash

import java.math.BigInteger

import encry.utils.LittleEndianBytes._
import org.bouncycastle.crypto.Digest
import org.bouncycastle.crypto.digests.Blake2bDigest
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ArrayBuffer

object Equihash {

  def nonceToLeBytes(nonce: BigInt): Array[Byte] =
    (for (i <- 0 to 7) yield leIntToByteArray((nonce >> 32 * i).intValue())).reduce(_ ++ _)


  def hashNonce[T <: Digest](digest: T, nonce: BigInt): T = {
    val arr: Array[Byte] = nonceToLeBytes(nonce)
    digest.update(arr, 0, arr.length)
    digest
  }

  def hashSolution[T <: Digest](digest: T, solution: EquihashSolution): T = {
    solution.ints map { hashXi(digest, _) }
    digest
  }

  def hashXi[T <: Digest](digest: T, xi: Int): T = {
    val arr: Array[Byte] = leIntToByteArray(xi)
    digest.update(arr, 0, arr.length)
    digest
  }

  private val byteSize: Int = 8

  def countLeadingZeroes(bytes: Array[Byte]): Int = (0 until byteSize * bytes.length).foldLeft(0.toByte) {
    case (res, i) if (bytes(i / byteSize) << i % byteSize & 0x80) == 0 => (res + 1).toByte
    case (res, _) => res
  }

  def hasCollision(ha: Array[Byte], hb: Array[Byte], i: Int, lenght: Int): Boolean = {
    ((i - 1) * lenght / 8 until i * lenght / 8).forall(j => ha(j) == hb(j))
  }

  def distinctIndices(a: Seq[Int], b: Seq[Int]): Boolean = !a.exists(v => b.contains(v))

  def xor(ha: Array[Byte], hb: Array[Byte]): Array[Byte] =
    for {(a, b) <- ha.zip(hb)} yield (a ^ b).toByte


  private val log: Logger = LoggerFactory.getLogger(getClass)
  private val wordSize: Int = 32

  def expandArray(inp: Array[Byte], outLen: Int, bitLen: Int, bytePad: Int = 0): Array[Byte] = {
    assert(bitLen >= 8 && wordSize >= 7 + bitLen)

    val outWidth: Int = (bitLen + 7) / 8 + bytePad
    assert(outLen == 8 * outWidth * inp.length / bitLen)
    val out: Array[Byte] = new Array[Byte](outLen)
    
    var accValue = BigInteger.ZERO

    inp.indices.foldLeft(0, 0) {

      case ((accBits, j), i) =>
      accValue = accValue.shiftLeft(8).and(BigInteger.ONE.shiftLeft(wordSize).subtract(BigInteger.ONE)).or(BigInteger.valueOf((inp(i) & 0xFF).toLong))
      val possibleAccBits: Int = accBits + 8

      if (possibleAccBits >= bitLen) {
        val correctAccBits: Int = possibleAccBits - bitLen
        (bytePad until outWidth).foreach(x =>
          out.update(j + x, {
            accValue.shiftRight(correctAccBits + (8 * (outWidth - x - 1)))
              .and(BigInteger.valueOf((1 << bitLen) - 1).shiftRight(8 * (outWidth - x - 1)).and(BigInteger.valueOf(0xFF)))
              .byteValue()
          })
        )
        (correctAccBits, j + outWidth)
      } else (possibleAccBits, j)
    }

    out
  }

  // Implementation of Basic Wagner's algorithm for the GBP
  def gbpBasic(digest: Blake2bDigest, n: Char, k: Char): Seq[EquihashSolution] = {
    val collisionLength: Int = n / (k + 1)
    val hashLength: Int = (k + 1) * ((collisionLength + 7) / 8)
    val indicesPerHashOutput: Int = 512 / n
    log.trace("Generating first list")
    //  1) Generate first list
    val tmpHash: Array[Byte] = new Array[Byte](digest.getDigestSize)
    var X = for {i <- (0 until Math.pow(2, collisionLength + 1).toInt).toVector} yield {
      val r: Int = i % indicesPerHashOutput
      if (r == 0) {
        //  X_i = H(I||V||x_i)
        val currDigest: Blake2bDigest = new Blake2bDigest(digest)
        hashXi(currDigest, i / indicesPerHashOutput)
        currDigest.doFinal(tmpHash, 0)
      }
      val d: Array[Byte] = tmpHash.slice(r * n / 8, (r + 1) * n / 8)
      val expanded: Array[Byte] = expandArray(d, hashLength, collisionLength)
      expanded -> Seq(i)
    }

    //  3) Repeat step 2 until 2n/(k+1) bits remain
    for (i <- 1 until k) {
      log.trace(s"Round $i")

      //  2a) Sort the list
      log.trace("- Sorting list")
      X = X.sortBy(_._1.toIterable)

      log.trace("- Finding collisions")
      var Xc: Vector[(Array[Byte], Seq[Int])] = Vector.empty[(Array[Byte], Seq[Int])]
      while (X.nonEmpty) {
        //  2b) Find next set of unordered pairs with collisions on first n/(k+1) bits
        val XSize: Int = X.size
        val j: Int = (1 until XSize).find( j => !hasCollision(X.last._1, X(XSize - 1 - j)._1, i, collisionLength)).getOrElse(XSize)

        //  2c) Store tuples (X_i ^ X_j, (i, j)) on the table
        for {
          l <- 0 until j - 1
          m <- l + 1 until j
        } {
          val X1l: (Array[Byte], Seq[Int]) = X(XSize - 1 - l)
          val X1m: (Array[Byte], Seq[Int]) = X(XSize - 1 - m)
          //  Check that there are no duplicate indices in tuples i and j
          if (distinctIndices(X1l._2, X1m._2)) {
            val concat: Seq[Int] = if (X1l._2.head < X1m._2.head) {
              X1l._2 ++ X1m._2
            } else {
              X1m._2 ++ X1l._2
            }
            Xc = Xc :+ (xor(X1l._1, X1m._1) -> concat)
          }
        }

        //  2d) Drop this set
        X = X.dropRight(j)
      }
      //  2e) Replace previous list with new list
      X = Xc
    }

    //  k+1) Find a collision on last 2n(k+1) bits
    log.trace("Final round:")
    log.trace("- Sorting list")

    X = X.sortBy(_._1.toIterable)

    log.trace("- Finding collisions")

    var solns = Vector.empty[EquihashSolution]

    while (X.nonEmpty) {
      val XSize: Int = X.length

      val j: Int = (1 until XSize).find( j => !(hasCollision(X.last._1, X(XSize - 1 - j)._1, k, collisionLength) &&
        hasCollision(X.last._1, X(XSize - 1 - j)._1, k + 1, collisionLength))).getOrElse(XSize)

      for {
        l <- 0 until j - 1
        m <- l + 1 until j
      } {
        val res: Array[Byte] = xor(X(XSize - 1 - l)._1, X(XSize - 1 - m)._1)
        if (countLeadingZeroes(res) == 8 * hashLength && distinctIndices(X(XSize - 1 - l)._2, X(XSize - 1 - m)._2)) {
          val p: Seq[Int] = if (X( XSize - 1 - l )._2.head < X( XSize - 1 - m )._2.head)
            X(XSize - 1 - l)._2 ++ X(XSize - 1 - m)._2
          else
            X(XSize - 1 - m)._2 ++ X(XSize - 1 - l)._2

          solns = solns :+ EquihashSolution(p)
        }
      }

      X = X.dropRight(j)
    }

    solns
  }

  /**
    * Generate n-bit word at specified index.
    *
    * @param n                Word length in bits
    * @param digestWithoutIdx digest without index
    * @param idx              word index
    * @return word
    */
  // https://github.com/str4d/zcash-pow/blob/master/test-pow.py
  def generateWord(n: Char, digestWithoutIdx: Blake2bDigest, idx: Int): BigInteger = {
    val bytesPerWord: Int = n / 8
    val wordsPerHash: Int = 512 / n

    val hidx: Int = idx / wordsPerHash
    val hrem: Int = idx % wordsPerHash

    val idxdata: Array[Byte] = leIntToByteArray(hidx)
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
    * @return Return True if solution is valid, False if not.
    */
  @SuppressWarnings(Array("NullParameter"))
  def validateSolution(n: Char, k: Char, personal: Array[Byte], header: Array[Byte], solutionIndices: IndexedSeq[Int]): Boolean = {

    assert(n > 1)
    assert(k >= 3)
    assert(n % 8 == 0)
    assert(n % (k + 1) == 0)

    val solutionLen: Int = Math.pow(2, k).toInt
    assert(solutionIndices.size == solutionLen)

    if (solutionIndices.toSet.size != solutionIndices.size) false
    else {
      val bytesPerWord: Int = n / 8
      val wordsPerHash: Int = 512 / n
      val outlen: Int = wordsPerHash * bytesPerWord

      val digest: Blake2bDigest = new Blake2bDigest(null, outlen, null, personal)
      digest.update(header, 0, header.length)

      val pairWiseCheck: Boolean = (0 until k).forall(s => {
        val d: Int = 1 << s
        (0 until solutionLen by 2 * d).forall(i =>
          if (solutionIndices(i) >= solutionIndices(i + d)) false
          else true
        )
      })

      val words: ArrayBuffer[BigInteger] =
        (0 until solutionLen).foldLeft(ArrayBuffer.empty[BigInteger]) {
          case (buffer, i) => buffer += generateWord(n, digest, solutionIndices(i))
        }

      val xorConditionsCheck: Boolean = {
        val bitsPerStage: Int = n / (k + 1)
        (0 until k).forall(s => {
          val d: Int = 1 << s
          (0 until solutionLen by 2 * d).forall( i => {
            val w: BigInteger = words(i).xor(words(i + d))
            if (w.shiftRight(n - (s + 1) * bitsPerStage) != BigInteger.ZERO) false
            else {
              words( i ) = w
              true
            }
          })
        })
      }

      words(0) == BigInteger.ZERO && pairWiseCheck && xorConditionsCheck
    }
  }
}