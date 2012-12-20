package info.sumito3478.math

import info.sumito3478.{ Dummy0, Dummy1, Dummy2, Dummy3 }
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.WrappedArray

trait CRC32 extends GenCRC32 {
  protected[this] val poly: Int = 0x04c11db7

  private[this] lazy val reversed: Int = {
    java.lang.Integer.reverse(poly)
  }

  protected[this] lazy val table: Array[Int] = {
    val ret = new Array[Int](256)
    new Range(7, -1, -1).map(math.pow(2, _).toInt).foldLeft[Int](1) {
      (crc0, i) =>
        val crc1 = (crc0 >>> 1) ^ (if ((crc0 & 1) == 0) 0 else reversed)
        new Range(0, 256, 2 * i) foreach {
          j =>
            ret(i + j) = crc1 ^ ret(j)
        }
        crc1
    }
    ret
  }

  private[this] lazy val tables: Array[Array[Int]] = {
    val ret = Array.ofDim[Int](8, 256)
    ret(0) = table
    0 until 256 foreach {
      i =>
        val crc = table(i)
        (1 until 8).foldLeft(crc) {
          (crc0, j) =>
            val crc1 = table(crc0 & 0xff) ^ (crc0 >>> 8)
            ret(j)(i) = crc1
            crc1
        }
    }
    ret
  }

  lazy val table1 = tables(1)

  lazy val table2 = tables(2)

  lazy val table3 = tables(3)

  lazy val table4 = tables(4)

  lazy val table5 = tables(5)

  lazy val table6 = tables(6)

  lazy val table7 = tables(7)

  override def crc(h: Int, input: Int): Int = {
    // interpret as a little endian binary, even on big endian platforms.
    val q = ~h ^ input
    ~(table3((q) & 255) ^ table2((q >> 8) & 255) ^ table1((q >> 16) & 255) ^ table0((q >> 24) & 255))
  }

  override def crc(h: Int, inputs: Iterator[Int])(implicit dummy: Dummy2): Int = {
    inputs.foldLeft(h) {
      (h, input) =>
        crc(h, input)
    }
  }

  override def crc(h: Int, input: Long): Int = {
    // interpret as a little endian binary, even on big endian platforms.
    val q0 = ~h ^ input.toInt
    val h0 = (table7((q0) & 255) ^ table6((q0 >> 8) & 255) ^ table5((q0 >> 16) & 255) ^ table4((q0 >> 24) & 255))
    val q1 = ((input >>> 32) & 0xffffffff).toInt
    ~(h0 ^ (table3((q1) & 255) ^ table2((q1 >> 8) & 255) ^ table1((q1 >> 16) & 255) ^ table0((q1 >> 24) & 255)))
  }

  override def crc(h: Int, inputs: Iterator[Long])(implicit dummy: Dummy3): Int = {
    inputs.foldLeft(h) {
      (h, input) =>
        crc(h, input)
    }
  }
}

object CRC32 extends CRC32 {
  override val poly = 0x04c11db7
}