package sumito3478

import java.nio.ByteBuffer
import java.nio.ByteOrder
import sumito3478.math.CityHash
import java.nio.CharBuffer
import java.math.{ BigInteger => JBigInt }

trait HashCoder[@specialized -A] {
  def hashCode(obj: A): Long
}

object HashCoder {
  def generateAnyHashCoderSource: String = {
    val coders = List(
      "Byte", "Int", "Short", "Char", "Long", "Float", "Double", "String",
      "JBitInt", "BitInt")
    coders map {
      coder =>
        s"case x: ${coder} => ${coder}HashCoder.hashCode(x)"
    } mkString "\n"
  }

  implicit object AnyHashCoder extends HashCoder[Any] {
    def hashCode(x: Any): Long = x match {
      case x: Byte => ByteHashCoder.hashCode(x)
      case x: Int => IntHashCoder.hashCode(x)
      case x: Short => ShortHashCoder.hashCode(x)
      case x: Char => CharHashCoder.hashCode(x)
      case x: Long => LongHashCoder.hashCode(x)
      case x: Float => FloatHashCoder.hashCode(x)
      case x: Double => DoubleHashCoder.hashCode(x)
      case x: String => StringHashCoder.hashCode(x)
      case x => {
        import CityHash._
        fmix(mur(x.hashCode, c._1))
      }
    }
  }

  implicit object ByteHashCoder extends HashCoder[Byte] {
    def hashCode(x: Byte): Long = {
      CityHash.cityHash64(x)
    }
  }

  implicit object IntHashCoder extends HashCoder[Int] {
    def hashCode(x: Int): Long = {
      CityHash.cityHash64(x)
    }
  }

  implicit object ShortHashCoder extends HashCoder[Short] {
    def hashCode(x: Short): Long = {
      CityHash.cityHash64(x)
    }
  }

  implicit object CharHashCoder extends HashCoder[Char] {
    def hashCode(x: Char): Long = {
      CityHash.cityHash64(x)
    }
  }

  implicit object LongHashCoder extends HashCoder[Long] {
    def hashCode(x: Long): Long = {
      CityHash.cityHash64(x)
    }
  }

  implicit object FloatHashCoder extends HashCoder[Float] {
    def hashCode(x: Float): Long = {
      CityHash.cityHash64(x)
    }
  }

  implicit object DoubleHashCoder extends HashCoder[Double] {
    def hashCode(x: Double): Long = {
      CityHash.cityHash64(x)
    }
  }

  implicit object StringHashCoder extends HashCoder[String] {
    def hashCode(x: String): Long = {
      val len = x.length * 2
      val buffer = ByteBuffer.allocate(len)
      buffer.order(ByteOrder.nativeOrder())
      buffer.asCharBuffer.put(x)
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      CityHash.cityHash64(new ByteBufferPointer(buffer, 0), len)
    }
  }

  implicit object JBigIntHashCoder extends HashCoder[JBigInt] {
    def hashCode(x: JBigInt): Long = {
      val data = x.toByteArray
      val buffer = ByteBuffer.allocate(data.length)
      buffer.put(data)
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      CityHash.cityHash64(new ByteBufferPointer(buffer, 0), data.length)
    }
  }

  implicit object BigIntHashCoder extends HashCoder[BigInt] {
    def hashCode(x: BigInt): Long = {
      JBigIntHashCoder.hashCode(x.bigInteger)
    }
  }
}
