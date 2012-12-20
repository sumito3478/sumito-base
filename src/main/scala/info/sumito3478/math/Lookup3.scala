package info.sumito3478.math

import info.sumito3478.collection.RichIterator
import info.sumito3478.util.control.breakable

/**
 * implementation of the Jenkins Hash Algorithm.
 *
 * @see http://burtleburtle.net/bob/c/lookup3.c
 */
object Lookup3 {
  /**
   * initialize the three 32bit integers for the Jenkins Hash Algorithm.
   *
   * @param length length of the input to be hashed.
   * @param seed seed for the hash calculation.
   */
  def init(length: Int, seed: Int): (Int, Int, Int) = {
    val ret = 0xdeadbeef + (length << 2) + seed
    (ret, ret, ret)
  }

  /**
   * mix three 32bit integers
   */
  def mix(x: Int, y: Int, z: Int): (Int, Int, Int) = {
    var a: Int = x
    var b: Int = y
    var c: Int = z
    a -= c; a ^= (c << 4) | (c >>> -4); c += b;
    b -= a; b ^= (a << 6) | (a >>> -6); a += c;
    c -= b; c ^= (b << 8) | (b >>> -8); b += a;
    a -= c; a ^= (c << 16) | (c >>> -16); c += b;
    b -= a; b ^= (a << 19) | (a >>> -19); a += c;
    c -= b; c ^= (b << 4) | (b >>> -4); b += a;
    (a, b, c)
  }

  /**
   * mix three 32bit integers for the final phase of hashing and return one integer.
   */
  def mixFinal(x: Int, y: Int, z: Int): Int = {
    var a: Int = x
    var b: Int = y
    var c: Int = z
    c ^= b; c -= (b << 14) | (b >>> -14);
    a ^= c; a -= (c << 11) | (c >>> -11);
    b ^= a; b -= (a << 25) | (a >>> -25);
    c ^= b; c -= (b << 16) | (b >>> -16);
    a ^= c; a -= (c << 4) | (c >>> -4);
    b ^= a; b -= (a << 14) | (a >>> -14);
    c ^= b; c -= (b << 24) | (b >>> -24);
    c
  }

  /**
   * hash integers produced by the iterator
   */
  def apply(input: Iterator[Int], length: Int, seed: Int): Int = {
    val it = input.lookAhead
    var (a, b, c) = init(length, seed)
    breakable {
      break =>
        while (true) {
          val la = it.lookAhead(4)
          val read = la.length
          if (read > 3) {
            a += la(0)
            b += la(1)
            c += la(2)
            val mixed = mix(a, b, c)
            a = mixed._1
            b = mixed._2
            c = mixed._3
            it.drop(3)
          } else {
            read match {
              case 3 => a += la(0); b += la(1); c += la(2); c = mixFinal(a, b, c)
              case 2 => a += la(0); b += la(1); c = mixFinal(a, b, c)
              case 1 => a += la(0); c = mixFinal(a, b, c)
              case 0 => ()
            }
            break
          }
        }
    }
    c
  }

  def apply(input: Iterator[Int], length: Int): Int = {
    apply(input, length, 0)
  }

  /**
   * hash integers contained in IndexedSeq.
   */
  def apply(input: IndexedSeq[Int], seed: Int): Int = {
    var rest = input.length
    var i = 0
    var (a, b, c) = init(rest, seed)
    while (rest > 3) {
      a += input(i)
      b += input(i + 1)
      c += input(i + 2)
      val mixed = mix(a, b, c)
      a = mixed._1
      b = mixed._2
      c = mixed._3
      rest -= 3
      i += 3
    }
    rest match {
      case 3 => a += input(i); b += input(i + 1); c += input(i + 2); c = mixFinal(a, b, c)
      case 2 => a += input(i); b += input(i + 1); c = mixFinal(a, b, c)
      case 1 => a += input(i); c = mixFinal(a, b, c)
    }
    c
  }

  def apply(input: IndexedSeq[Int]): Int = {
    apply(input, 0)
  }
}