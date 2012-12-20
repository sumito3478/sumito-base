package info.sumito3478.math

import info.sumito3478.{ Dummy0, Dummy2, Dummy3 }

trait GenCRC32 {
  protected[this] val table: Array[Int]

  val init: Int = 0

  lazy val table0 = table

  def crc(h: Int, input: Byte): Int = {
    ~((~h >>> 8) ^ table0((~h ^ input) & 0xff))
  }

  def crc(h: Int, inputs: Iterator[Byte])(implicit dummy: Dummy0): Int = {
    inputs.foldLeft(h) {
      (h, input) =>
        crc(h, input)
    }
  }

  def crc(h: Int, input: Int): Int = {
    // interpret as a little endian binary, even on big endian platforms.
    crc(crc(crc(crc(h, (input & 0xff).toByte), ((input >>> 8) & 0xff).toByte), ((input >>> 16) & 0xff).toByte), ((input >>> 24) & 0xff).toByte)
  }

  def crc(h: Int, inputs: Iterator[Int])(implicit dummy: Dummy2): Int = {
    inputs.foldLeft(h) {
      (h, input) =>
        crc(h, input)
    }
  }

  def crc(h: Int, input: Long): Int = {
    // interpret as a little endian binary, even on big endian platforms.
    crc(crc(h, (input & 0xffffffff).toInt), ((input >>> 32) & 0xffffffff).toInt)
  }

  def crc(h: Int, inputs: Iterator[Long])(implicit dummy: Dummy3): Int = {
    inputs.foldLeft(h) {
      (h, input) =>
        crc(h, input)
    }
  }
}