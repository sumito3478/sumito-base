package sumito3478

import java.nio.ByteBuffer
import java.nio.ByteOrder
import sumito3478.math.CityHash

trait HashCoder[@specialized -A] {
  def hashCode(obj: A): Long
}

object HashCoder {
  implicit object AnyHashCoder extends HashCoder[Any] {
    def hashCode(x: Any): Long = {
      val h = x.hashCode
      ((h >>> 1).toLong << 1) + (h & 0x1)
    }
  }

  implicit object LongHashCoder extends HashCoder[Long] {
    def hashCode(x: Long): Long = {
      val mul = CityHash.k._2 + 8 * 2
      val a = x + CityHash.k._2
      val b = x
      val c = CityHash.rotate(b, 37) * mul + a
      val d = (CityHash.rotate(a, 25) + b) * mul
      CityHash.hashLen16(c, d, mul)
    }
  }

  implicit object IntHashCoder extends HashCoder[Int] {
    def hashCode(x: Int): Long = {
      val mul = CityHash.k._2 + 4 * 2
      val a = x
      CityHash.hashLen16(4 + (a << 3), x, mul)
    }
  }

  implicit object ShortHashCoder extends HashCoder[Short] {
    def hashCode(x: Short): Long = {
      val mul = CityHash.k._2 + 4 * 2
      val a = x
      CityHash.hashLen16(4 + (a << 3), x, mul)
    }
  }

}
