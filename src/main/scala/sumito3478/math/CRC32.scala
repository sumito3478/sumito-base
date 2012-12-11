package sumito3478.math

import sumito3478.{Dummy0, Dummy1, Dummy2, Dummy3}
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.WrappedArray

trait CRC32 {
  val poly: Int

  val init: Int = 0

  lazy val reversed: Int = {
    java.lang.Integer.reverse(poly)
  }

  lazy val tables: IndexedSeq[IndexedSeq[Int]] = {
    val buf = Array.ofDim[Int](8, 256)
    var crc = 1
    var i = 128
    while (i > 0) {
      crc = (crc >>> 1) ^ (if ((crc & 1) == 0) 0 else reversed)
      var j = 0
      while (j < 256) {
        buf(0)(i + j) = crc ^ buf(0)(j)
        j += 2 * i
      }
      i >>>= 1
    }
    i = 0
    while (i < 256) {
      crc = buf(0)(i)
      var j = 1
      while (j < 8) {
        crc = buf(0)(crc & 0xff) ^ (crc >>> 8)
        buf(j)(i) = crc
        j += 1
      }
      i += 1
    }
    val tmp = buf.map(arr => (new VectorBuilder[Int] ++= arr).result)
    (new VectorBuilder[IndexedSeq[Int]] ++= tmp).result
  }

  lazy val table0 = tables(0)

  lazy val table1 = tables(1)

  lazy val table2 = tables(2)

  lazy val table3 = tables(3)

  lazy val table4 = tables(4)

  lazy val table5 = tables(5)

  lazy val table6 = tables(6)

  lazy val table7 = tables(7)

  def crc(h: Int, input: Byte): Int = {
    ~ ((~ h >>> 8) ^ table0((~ h ^ input) & 0xff))
  }
  
  def crc(h: Int, inputs: Iterator[Byte])(implicit dummy: Dummy0): Int = {
    inputs.foldLeft(h) {
      (h, input) =>
        crc(h, input)
    }
  }

  def crc(h: Int, input: Short): Int = {
    // interpret as a little endian binary, even on big endian platforms.
    crc(crc(h, input.toByte), ((input >>> 8) & 0xff).toByte)
  }
  
  def crc(h: Int, inputs: Iterator[Short])(implicit dummy: Dummy1): Int = {
    inputs.foldLeft(h) {
      (h, input) =>
        crc(h, input)
    }
  }

  def crc(h: Int, input: Int): Int = {
    // interpret as a little endian binary, even on big endian platforms.
    val q = ~h ^ input
    ~ (table3((q) & 255) ^ table2((q >> 8) & 255) ^ table1((q >> 16) & 255) ^ table0((q >> 24) & 255))
  }
  
  def crc(h: Int, inputs: Iterator[Int])(implicit dummy: Dummy2): Int = {
    inputs.foldLeft(h) {
      (h, input) =>
        crc(h, input)
    }
  }

  def crc(h: Int, input: Long): Int = {
    // interpret as a little endian binary, even on big endian platforms.
    val q0 = ~ h ^ input.toInt
    val h0 =  (table7((q0) & 255) ^ table6((q0 >> 8) & 255) ^ table5((q0 >> 16) & 255) ^ table4((q0 >> 24) & 255))
    val q1 = ((input >>> 32) & 0xffffffff).toInt
    ~ (h0 ^ (table3((q1) & 255) ^ table2((q1 >> 8) & 255) ^ table1((q1 >> 16) & 255) ^ table0((q1 >> 24) & 255)))
  }
  
  def crc(h: Int, inputs: Iterator[Long])(implicit dummy: Dummy3): Int = {
    inputs.foldLeft(h) {
      (h, input) =>
        crc(h, input)
    }
  }
}

object CRC32 extends CRC32 {
  val poly = 0x04c11db7
}