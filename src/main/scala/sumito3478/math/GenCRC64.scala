package sumito3478.math

import sumito3478.{ Dummy0, Dummy2, Dummy3 }

trait GenCRC64 {
  protected[this] val table: Array[Long]

  val init: Long = 0

  lazy val table0 = table

  def crc(h: Long, input: Byte): Long = {
    ~((~h >>> 8) ^ table0(((~h ^ input) & 0xff).toInt))
  }

  def crc(h: Long, inputs: Iterator[Byte])(implicit dummy: Dummy0): Long = {
    inputs.foldLeft(h) {
      (h, input) =>
        crc(h, input)
    }
  }

  def crc(h: Long, input: Int): Long = {
    // interpret as a little endian binary, even on big endian platforms.
    crc(crc(crc(crc(h, (input & 0xff).toByte), ((input >>> 8) & 0xff).toByte), ((input >>> 16) & 0xff).toByte), ((input >>> 24) & 0xff).toByte)
  }

  def crc(h: Long, inputs: Iterator[Int])(implicit dummy: Dummy2): Long = {
    inputs.foldLeft(h) {
      (h, input) =>
        crc(h, input)
    }
  }

  def crc(h: Long, input: Long): Long = {
    // interpret as a little endian binary, even on big endian platforms.
    crc(crc(h, (input & 0xffffffff).toInt), ((input >>> 32) & 0xffffffff).toInt)
  }

  def crc(h: Long, inputs: Iterator[Long])(implicit dummy: Dummy3): Long = {
    inputs.foldLeft(h) {
      (h, input) =>
        crc(h, input)
    }
  }
}