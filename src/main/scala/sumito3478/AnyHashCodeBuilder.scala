package sumito3478

import sumito3478.math.CRC64ECMA
import scala.collection.GenIterable

class AnyHashCodeBuilder {
  private[this] val CRC = CRC64ECMA

  private[this] var crc: Long = CRC.init
  
  def result: Int = {
    (crc & 0xffffffff).toInt
  }
  
  def ++=(input: Iterator[Byte])(implicit dummy: Dummy0): this.type = {
    crc = CRC.crc(crc, input)
    this
  }
  
  def ++=(input: GenIterable[Byte])(implicit dummy: Dummy0): this.type = {
    ++=(input.iterator)
    this
  }
  
  def ++=(input: Byte*)(implicit dummy: Dummy0): this.type = {
    ++=(input)
    this
  }
  
  def ++=(input: Iterator[Int])(implicit dummy: Dummy1): this.type = {
    crc = CRC.crc(crc, input)
    this
  }
  
  def ++=(input: GenIterable[Int])(implicit dummy: Dummy1): this.type = {
    ++=(input.iterator)
    this
  }
  
  def ++=(input: Int*)(implicit dummy: Dummy1): this.type = {
    ++=(input.iterator)
    this
  }
  
  def ++=(input: Iterator[Long])(implicit dummy: Dummy2): this.type = {
    crc = CRC.crc(crc, input)
    this
  }
  
  def ++=(input: GenIterable[Long])(implicit dummy: Dummy2): this.type = {
    ++=(input.iterator)
    this
  }
  
  def ++=(input: Long*)(implicit dummy: Dummy2): this.type = {
    ++=(input.iterator)
    this
  }
}