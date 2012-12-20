package info.sumito3478.math

import info.sumito3478.{ Dummy0, Dummy1, Dummy2, Dummy3 }
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.WrappedArray

trait CRC64 extends GenCRC64 {
  protected[this] val poly: Long = 0x000000000000001bL

  private[this] lazy val reversed: Long = {
    java.lang.Long.reverse(poly)
  }

  protected[this] lazy val table: Array[Long] = {
    val ret = new Array[Long](256)
    new Range(7, -1, -1).map(math.pow(2, _).toInt).foldLeft[Long](1) {
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

  private[this] lazy val tables: Array[Array[Long]] = {
    val ret = Array.ofDim[Long](8, 256)
    ret(0) = table
    0 until 256 foreach {
      i =>
        val crc = table(i)
        (1 until 8).foldLeft(crc) {
          (crc0, j) =>
            val crc1 = table((crc0 & 0xff).toInt) ^ (crc0 >>> 8)
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

  override def crc(h: Long, input: Int): Long = {
    // interpret as a little endian binary, even on big endian platforms.
    val q = ~h ^ input
    ~(table3(((q) & 0xff).toInt) ^ table2(((q >>> 8) & 0xff).toInt) ^ (q >>> 32) ^ table1(((q >>> 16) & 0xff).toInt) ^ table0(((q >>> 24) & 0xff).toInt))
  }

  override def crc(h: Long, inputs: Iterator[Int])(implicit dummy: Dummy2): Long = {
    inputs.foldLeft(h) {
      (h, input) =>
        crc(h, input)
    }
  }
}

object CRC64 extends CRC64 {
  override val poly = 0x000000000000001bL
}