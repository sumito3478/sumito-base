package sumito3478.math

import java.nio.ByteBuffer

object CityHash {
  class ArrayView[@specialized A](val intern: Array[A], val pos: Int) {
    def apply(idx: Int): A = {
      intern(idx + pos)
    }
    
    def update(idx: Int, elem: A): Unit = {
      intern(idx + pos) = elem
    }
    
    def length = {
      intern.length - pos
    }
  }
  
  object ArrayView {
    def apply[@specialized A](intern: ArrayView[A], pos: Int): ArrayView[A] = {
      new ArrayView(intern.intern, intern.pos + pos)
    }
    
    def apply[@specialized A](intern: Array[A], pos: Int): ArrayView[A] = {
      new ArrayView(intern, pos)
    }
  }
  
  val kMul: Long = 0x9ddfea08eb382d69L

  def hash128to64(u: Long, v: Long): Long = {
    var a = (u ^ v) * kMul
    a ^= (a >>> 47)
    var b: Long = (v ^ a) * kMul
    b ^= (b >>> 47)
    b *= kMul
    b
  }

  def hash128to64(x: Cent): Long = {
    hash128to64(x.low, x.high)
  }

  def fetch64(xs: ArrayView[Byte]): Long = {
    (xs(7) << 56) +
      ((xs(6) & 0xff) << 48) +
      ((xs(5) & 0xff) << 40) +
      ((xs(4) & 0xff) << 32) +
      ((xs(3) & 0xff) << 24) +
      ((xs(2) & 0xff) << 16) +
      ((xs(1) & 0xff) << 8) +
      ((xs(0) & 0xff) << 0)
  }

  def fetch32(xs: ArrayView[Byte]): Int = {
    ((xs(3) & 0xff) << 24) +
      ((xs(2) & 0xff) << 16) +
      ((xs(1) & 0xff) << 8) +
      ((xs(0) & 0xff) << 0)
  }

  /**
   * Some primes between 2^63 and 2^64 for various uses.
   */
  object k {
    val _0: Long = 0xc3a5c85c97cb3127L
    val _1: Long = 0xb492b66fbe98f273L
    val _2: Long = 0x9ae16a3b2f90404fL
  }

  /**
   * Magic numbers for 32bit hashing. Copied from Murmur3.
   */
  object c {
    val _1: Int = 0xcc9e2d51
    val _2: Int = 0x1b873593
  }
  
  /**
   * A 32-bit to 32-bit integer hash copied from Murmur3.
   */
  def fmix(x: Int): Int = {
    var h = x
    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16
    h
  }
  
  def rotate32(x: Int, shift: Int): Int = {
    if(shift == 0) x else ((x >>> shift) | (x << (32 - shift)))
  }
 
  /**
   * Helper from Murmur3 for combining two 32bit values.
   */
  def mur(x: Int, y: Int): Int = {
    var a = x
    var h = y
    a *= c._1
    a = rotate32(a, 17)
    a *= c._2
    h ^= a
    h = rotate32(h, 19)
    h * 5 + 0xe6546b64
  }
//  
//  def hash32Len13to24(xs: ArrayView[Byte]): Int = {
//    val len = xs.length
//    val a = fetch32(ArrayView(xs, len >> 1))
//    val b = fetch32(ArrayView(xs, 4))
//    val c = fetch32(ArrayView(xs, len - 8))
//    val d = fetch32(ArrayView(xs, len >> 1))
//    val e = fetch32(ArrayView(xs, 0))
//    val f = fetch32(ArrayView(xs, len - 4))
//    val h = len
//    fmix(mur(f, mur(e, mur(d, mur(c, mur(b, mur(a, h)))))))
//  }
//  
//  def hash32Len0to4(xs: ArrayView[Byte]): Int = {
//    var b: Int = 0
//    var c: Int = 9
//    0 until xs.length foreach {
//      i =>
//        b = b * this.c._1 + xs(i)
//        c ^= b
//    }
//    fmix(mur(b, mur(xs.length, c)))
//  }
//  
//  def hash32Len5to12(xs: ArrayView[Byte]): Int = {
//    val len = xs.length
//    var a = len
//    var b = len * 5
//    var c = 9
//    var d = b
//    a += fetch32(ArrayView(xs, 0))
//    b += fetch32(ArrayView(xs, len - 4))
//    c += fetch32(ArrayView(xs, (len >> 1) & 4))
//    fmix(mur(c, mur(b, mur(a, d))))
//  }
//  
//  def cityHash32(xs: Array[Byte]): Int = {
//    val len = xs.length
//    len match {
//      case len if len <= 4 => hash32Len0to4(ArrayView(xs, 0))
//      case len if len <=12 => hash32Len5to12(ArrayView(xs, 0))
//      case len if len <= 24 => hash32Len13to24(ArrayView(xs, 0))
//      case len => {
//        var h = len
//        var g = c._1 * len
//        var f = g
//        var a0 = rotate32(fetch32(ArrayView(xs, len - 4)) * c._1, 17) * c._2
//        var a1 = rotate32(fetch32(ArrayView(xs, len - 8)) * c._1, 17) * c._2
//        var a2 = rotate32(fetch32(ArrayView(xs, len - 16)) * c._1, 17) * c._2
//        var a3 = rotate32(fetch32(ArrayView(xs, len - 12)) * c._1, 17) * c._2
//        var a4 = rotate32(fetch32(ArrayView(xs, len - 20)) * c._1, 17) * c._2
//        h ^= a0
//        h = rotate32(h, 19)
//        h = h * 5 + 0xe6546b64
//        h ^= a2
//        h = rotate32(h, 19)
//        h = h * 5 + 0xe6546b64
//        g ^= a1
//        g = rotate32(h, 19)
//        g = g * 5 + 0xe6546b64
//        g ^= a3
//        g = rotate32(h, 19)
//        g = g * 5 + 0xe6546b64
//        f ^= a4
//        f = rotate32(h, 19)
//        f = f * 5 + 0xe6546b64
//        0
//      }
//    }
//  }

  def rotate(x: Long, shift: Int): Long = {
    if (shift == 0) x else ((x >>> shift) | (x << (64 - shift)))
  }

  def shiftMix(x: Long): Long = {
    x ^ (x >>> 47)
  }
//
//  def hashLen0to16(xs: Array[Byte]): Long = {
//    val len = xs.length
//    len match {
//      case len if len > 8 => {
//        val a = fetch64(new ArrayView(xs, 0))
//        val b = fetch64(new ArrayView(xs, len - 8))
//        hash128to64(a, b) ^ b
//      }
//    }
//
//  }

  def hashLen16(u: Long, v: Long): Long = {
    hash128to64(u, v)
  }
  
  def hashLen16(u: Long, v: Long, mul: Long): Long = {
    // Murmur-inspired hashing.
    var a = (u ^ v) * mul
    a ^= (a >>> 47)
    var b = (v ^ a) * mul
    b ^= (b >>> 47)
    b *= mul
    b
  }
  
  def hashLen0to16(xs: ByteBuffer, len: Int): Long = {
    val s = xs.position
    len match {
      case len if len >= 8 => {
        val mul = k._2 + len * 2
        val a = xs.getLong(s) + k._2
        val b = xs.getLong(s + len - 8)
        val c = rotate(b, 37) * mul + a
        val d = (rotate(a, 25) + b) * mul
        hashLen16(c, d, mul)
      }
      case len if len >= 4 => {
        val mul = k._2 + len * 2
        val a = xs.getLong(s)
        hashLen16(len + (a << 3), xs.getInt(s + len - 4), mul)
      }
      case len if len > 0 => {
        val a = xs.get(s)
        val b = xs.get(s + (len >>> 1))
        val c = xs.get(s + len - 1)
        val y = a.toInt + (b.toInt << 8)
        val z = len + (c.toInt << 2)
        shiftMix(y * k._2 ^ z * k._0) * k._2
      }
      case _ => k._2
    }
  }
  
  def hashLen17to32(xs: ByteBuffer, len: Int): Long = {
    val s = xs.position
    val mul = k._2 + len * 2
    val a = xs.getLong(s) + k._1
    val b = xs.getLong(s + 8)
    val c = xs.getLong(s + len - 8) * mul
    val d = xs.getLong(s + len - 16) * k._2
    hashLen16(rotate(a + b, 43) + rotate(c, 30) + d, a + rotate(b + k._2, 18) + c, mul)
  }
  
  /**
   * Return a 16-byte hash for 48 bytes. Quick and dirty.
   * 
   * @note Callers do best to use "random-looking" values for a and b.
   */
  def weakHashLen32WithSeeds(w: Long, x: Long, y: Long, z: Long, a0: Long, b0: Long): Cent = {
    var a = a0
    var b = b0
    a += w
    b = rotate(b + a + z, 21)
    val c = a
    a += x
    a += y
    b += rotate(a, 44)
    new Cent(a + z, b + c)
  }
  
  /**
   * Return a 16-byte hash for xs.get(0) ... xs.get(31), a, and b. Quick and dirty.
   */
  def weakHashLen32WithSeeds(xs: ByteBuffer, a: Long, b: Long): Cent = {
    val s = xs.position
    weakHashLen32WithSeeds(xs.getLong(s),
        xs.getLong(s + 8), 
        xs.getLong(s + 16),
        xs.getLong(s + 24),
        a,
        b)
  }
  
  /**
   * A subroutine for cityHash128. Returns a decent 128-bit hash for strings
   * of any length representable in signed 32-bit integer. Based on City and Murmur.
   */
  def cityMurmur(xs: ByteBuffer, len: Int, seed: Cent): Cent = {
    var s = xs.position
    println(s"s = $s")
    var a = seed.low
    var b = seed.high
    var c = 0L
    var d = 0L
    var l = len - 16
    if (l <= 0) {
      a = shiftMix(a * k._1) * k._1
      c = b * k._1 + hashLen0to16(xs, len)
      d = shiftMix(a + (if(len >= 8) xs.getLong(s) else c))
    } else {
      println(s"s = $s")
      c = hashLen16(xs.getLong(s + len - 8) + k._1, a)
      d = hashLen16(b + len, c + xs.getLong(s + len - 16))
      a += d
      do {
        a ^= shiftMix(xs.getLong(s) * k._1) * k._1
        a *= k._1
        b ^= a
        c ^= shiftMix(xs.getLong(s + 8) * k._1) * k._1
        c *= k._1
        d ^= c
        s += 16
        l -= 16
      } while(l > 0)
    }
    a = hashLen16(a, c)
    b = hashLen16(d, b)
    new Cent(a ^ b, hashLen16(b, a))
  }
  
  def cityHash128WithSeed(xs: ByteBuffer, length: Int, seed: Cent): Cent = {
    var len = length
    if(len < 128) {
      cityMurmur(xs, len, seed)
    } else {
      var s = xs.position
      var v = new MutableCent(0, 0)
      var w = new MutableCent(0, 0)
      var x = seed.low
      var y = seed.high
      var z = len * k._1
      v.low = rotate(y ^ k._1, 49) * k._1 + xs.getLong(s)
      v.high = rotate(v.low, 42) * k._1 + xs.getLong(s + 8)
      w.low = rotate(y + z, 35) * k._1 + x
      w.high = rotate(x + xs.getLong(s + 88), 53) * k._1
      do {
        x = rotate(x + y + v.low + xs.getLong(s + 8), 37) * k._1
        y = rotate(y + v.high + xs.getLong(s + 48), 42) * k._1
        x ^= w.high
        y += v.low + xs.getLong(s + 40)
        z = rotate(z + w.low, 33) * k._1
        xs.position(s)
        v = MutableCent(weakHashLen32WithSeeds(xs, v.high * k._1, x + w.low))
        xs.position(s + 32)
        w = MutableCent(weakHashLen32WithSeeds(xs, z + w.high, y + xs.getLong(s + 16)))
        var tmp = z
        z = x
        x = tmp
        s += 64
        xs.position(s)
        x = rotate(x + y + v.low + xs.getLong(s + 8), 37) * k._1
        y = rotate(y + v.high + xs.getLong(s + 48), 42) * k._1
        x ^= w.high
        y += v.low + xs.getLong(s + 40)
        z = rotate(z + w.low, 33) * k._1
        xs.position(s)
        v = MutableCent(weakHashLen32WithSeeds(xs, v.high * k._1, x + w.low))
        xs.position(s + 32)
        w = MutableCent(weakHashLen32WithSeeds(xs, z + w.high, y + xs.getLong(s + 16)))
        tmp = z
        z = x
        x = tmp
        s += 64
        len -= 128
      } while(len >= 128)
      x += rotate(v.low + z, 49) * k._0
      y = y * k._0 + rotate(w.high, 37)
      z = z * k._0 + rotate(w.low, 27)
      w.low *= 9
      v.low *= k._0
      var tail_done = 0
      while(tail_done < len) {
        tail_done += 32
        y = rotate(x + y, 42) * k._0 + v.high
        w.low += xs.getLong(s + len - tail_done + 16)
        x = x * k._0 + w.low
        z += w.high + xs.getLong(s + len - tail_done)
        w.high += v.low
        xs.position(s + len - tail_done)
        v = MutableCent(weakHashLen32WithSeeds(xs, v.low + z, v.high))
        v.low *= k._0
      }
      x = hashLen16(x, v.low)
      y = hashLen16(y + z, w.low)
      new Cent(hashLen16(x + v.high, w.high) + y, hashLen16(x + w.high, y + v.high))
    }
  }
  
  def cityHash128(xs: ByteBuffer, len: Int): Cent = {
    val s = xs.position
    if(len >= 16) {
      xs.position(s + 16)
      cityHash128WithSeed(xs, len - 16, new Cent(xs.getLong(s), xs.getLong(s + 8) + k._0))
    } else {
      cityHash128WithSeed(xs, len, new Cent(k._0, k._1))
    }
  }
}