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
    val arr = nonceToLeBytes(nonce)
    digest.update(arr, 0, arr.length)
    digest
  }

  def hashSolution[T <: Digest](digest: T, solution: EquihashSolution): T = {
    solution.ints.map(hashXi(digest, _))
    digest
  }

  def hashXi[T <: Digest](digest: T, xi: Int): T = {
    val arr = leIntToByteArray(xi)
    digest.update(arr, 0, arr.length)
    digest
  }

  private val byteSize = 8

  def countLeadingZeroes(bytes: Array[Byte]): Int = (0 until byteSize * bytes.length).foldLeft(0.toByte) {
    case (res, i) if (bytes(i / byteSize) << i % byteSize & 0x80) == 0 => (res + 1).toByte
    case (res, _) => res
  }

  def hasCollision(ha: Array[Byte], hb: Array[Byte], i: Int, lenght: Int): Boolean =
    ((i - 1) * lenght / 8 until i * lenght / 8).forall(j => ha(j) == hb(j))

  def distinctIndices(a: Seq[Int], b: Seq[Int]): Boolean = !a.exists(v => b.contains(v))

  def xor(ha: Array[Byte], hb: Array[Byte]): Array[Byte] =
    for {(a, b) <- ha.zip(hb)} yield (a ^ b).toByte

  private val log: Logger = LoggerFactory.getLogger(getClass)
  private val wordSize: Int = 32
  private val wordMask: BigInteger = BigInteger.ONE.shiftLeft(wordSize).subtract(BigInteger.ONE)
  private val byteMask: BigInteger = BigInteger.valueOf(0xFF)

  def expandArray(inp: Array[Byte], outLen: Int, bitLen: Int, bytePad: Int = 0): Array[Byte] = {

    assert(bitLen >= 8 && wordSize >= 7 + bitLen)

    val outWidth: Int = (bitLen + 7) / 8 + bytePad

    assert(outLen == 8 * outWidth * inp.length / bitLen)

    inp.indices.foldLeft(0, 0, BigInteger.ZERO, new Array[Byte](outLen)) {

      case ((j, accBits, accValue, out), i) =>
        val newAcc: BigInteger = accValue.shiftLeft(8).and(wordMask).or(BigInteger.valueOf((inp(i) & 0xFF).toLong))
        val possibleAccBits: Int = accBits + 8
        val (newJ: Int, newAccBits: Int) = if (possibleAccBits >= bitLen) {
          val correctAccBits: Int = possibleAccBits - bitLen
          (bytePad until outWidth).foreach(x =>
            out.update(j + x,
              accValue.shiftRight( accBits + (8 * (outWidth - x - 1)) )
                .add(BigInteger.valueOf((1 << bitLen) - 1).shiftRight(8 * (outWidth - x - 1)).and(byteMask))
                .byteValue()
            )
          )
          (j + outWidth, correctAccBits)
        } else (j, possibleAccBits)
        (newJ, newAccBits, newAcc, out)
    }._4
  }

  def compressArray(inp: Array[Byte], outLen: Int, bitLen: Int, bytePad: Int = 0): Array[Byte] = {
    assert(bitLen >= 8 && wordSize >= 7 + bitLen)

    val inWidth = (bitLen + 7) / 8 + bytePad
    assert(outLen == bitLen * inp.length / (8 * inWidth))

    (0 until outLen).foldLeft(0, 0, BigInteger.ZERO, new Array[Byte](outLen)) {

      case ((j, accBits, accValue, out), i) =>
        val (newJ, newAccBits, newAccValue) = if (accBits < 8) {
          val possibleAccValue: BigInteger =
            (bytePad until inWidth).foldLeft(accValue.shiftLeft(bitLen).and(wordMask).or(BigInteger.valueOf(inp(j).toLong))) {
              case (newAccVal, x) =>
                val b = BigInteger.valueOf(inp(j + x)).and(BigInteger.valueOf((1 << bitLen) - 1).shiftRight(8 * (inWidth - x - 1))
                  .and(BigInteger.valueOf(0xFF)))
                  .shiftLeft(8 * (inWidth - x - 1))
                newAccVal.or(b)
            }
          (j + inWidth, accBits + bitLen, possibleAccValue)
        } else (j, accBits, accValue)

        out(i) = accValue.shiftRight(accBits).and(BigInteger.valueOf(0xFF)).byteValue()

        (newJ, newAccBits - 8, newAccValue, out)
    }._4
  }

  // Implementation of Basic Wagner's algorithm for the GBP
  def gbpBasic(digest: Blake2bDigest, n: Char, k: Char): Seq[EquihashSolution] = {
    val collisionLength = n / (k + 1)
    val hashLength = (k + 1) * ((collisionLength + 7) / 8)
    val indicesPerHashOutput = 512 / n
    log.trace("Generating first list")
    //  1) Generate first list
    val tmpHash = new Array[Byte](digest.getDigestSize)
    val X: Seq[(Array[Byte], Seq[Int])] = for {i <- (0 until Math.pow(2, collisionLength + 1).toInt).toVector} yield {
      val r = i % indicesPerHashOutput
      if (r == 0) {
        //  X_i = H(I||V||x_i)
        val currDigest = new Blake2bDigest(digest)
        hashXi(currDigest, i / indicesPerHashOutput)
        currDigest.doFinal(tmpHash, 0)
      }
      val d = tmpHash.slice(r * n / 8, (r + 1) * n / 8)
      val expanded = expandArray(d, hashLength, collisionLength)
      expanded -> Seq(i)
    }
    
    //  3) Repeat step 2 until 2n/(k+1) bits remain

    val xOn3step = (1 until k).foldLeft(X) {

      case (x, i) =>
        log.trace(s"Round $i")
        //  2a) Sort the list
        log.trace("- Finding collisions")

        x.indices.foldLeft(Vector[(Array[Byte], Seq[Int])](), x.sortBy(_._1.toIterable)) {

          case ((xC, xForRound), _) =>

            val xSize = xForRound.length
            val j = (1 until xSize).find(j => !hasCollision(x.last._1, x(xSize - 1 - j)._1, i, collisionLength)).getOrElse(xSize)
            val newXc = (0 until j - 1).foldLeft(Vector[(Array[Byte], Seq[Int])]()) {

              case (xCforRound, l) =>
                val m = l + 1
                val X1l = x(xSize - 1 - l)
                val X1m = x(xSize - 1 - m)
                //  Check that there are no duplicate indices in tuples i and j
                if (distinctIndices(X1l._2, X1m._2)) {
                  val concat = if (X1l._2.head < X1m._2.head) {
                    X1l._2 ++ X1m._2
                  } else {
                    X1m._2 ++ X1l._2
                  }
                  xCforRound :+ (xor(X1l._1, X1m._1) -> concat)
                } else xCforRound
            }
            (newXc, xForRound.dropRight(1))
        }._1
    }

    //  k+1) Find a collision on last 2n(k+1) bits
    log.trace("Final round:")
    log.trace("- Sorting list")

    val xOn4Step = xOn3step.sortBy(_._1.toIterable)

    log.trace("- Finding collisions")

    xOn4Step.indices.foldLeft(xOn4Step, Seq[EquihashSolution]()) {

      case ((x, solutions), _) =>
        
        val xSize: Int = x.length
        val j: Int = (1 until xSize).find( j => !(hasCollision(x.last._1, x(xSize - 1 - j)._1, k, collisionLength) &&
          hasCollision(x.last._1, x(xSize - 1 - j)._1, k + 1, collisionLength))).getOrElse(xSize)
        
        val newSolutions = (0 until j - 1).foldLeft(Seq[EquihashSolution]()) {

          case (solutionsSeq, l) =>
            val m = l + 1
            val res = xor(x(xSize - 1 - l)._1, x(xSize - 1 - m)._1)
            if (countLeadingZeroes(res) == 8 * hashLength && distinctIndices(x(xSize - 1 - l)._2, x(xSize - 1 - m)._2)) {
              val p = if (x( xSize - 1 - l )._2.head < x( xSize - 1 - m )._2.head) {
                x(xSize - 1 - l)._2 ++ x(xSize - 1 - m)._2
              } else {
                x(xSize - 1 - m)._2 ++ x(xSize - 1 - l)._2
              }
              solutionsSeq :+ EquihashSolution(p)
            } else solutionsSeq
        }
        
        (x.dropRight(1), solutions ++ newSolutions)
    }._2
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

    val solutionLen = Math.pow(2, k).toInt
    assert(solutionIndices.size == solutionLen)

    // Check for duplicate indices.
    if (solutionIndices.toSet.size != solutionIndices.size) false
    else {
      // Generate hash words.
      val bytesPerWord = n / 8
      val wordsPerHash = 512 / n
      val outlen = wordsPerHash * bytesPerWord

      val digest = new Blake2bDigest(null, outlen, null, personal)
      digest.update(header, 0, header.length)

      // Check pair-wise ordening of indices.
      for (s <- 0 until k) {
        val d = 1 << s
        for (i <- 0 until solutionLen by 2 * d) {
          if (solutionIndices(i) >= solutionIndices(i + d)) {
            return false
          }
        }
      }

      val words = ArrayBuffer.empty[BigInteger]
      for (i <- 0 until solutionLen) {
        words += generateWord(n, digest, solutionIndices(i))
      }

      // Check XOR conditions.
      val bitsPerStage = n / (k + 1)
      for (s <- 0 until k) {
        val d = 1 << s
        for (i <- 0 until solutionLen by 2 * d) {
          val w = words(i).xor(words(i + d))
          if (w.shiftRight(n - (s + 1) * bitsPerStage) != BigInteger.ZERO) {
            return false
          }
          words(i) = w
        }
      }

      // Check final sum zero.
      words(0) == BigInteger.ZERO
    }
  }
}
