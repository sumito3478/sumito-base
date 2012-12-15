package sumito3478

import scala.{ math => smath }
import com.google.common.primitives.SignedBytes
import com.google.common.primitives.Shorts
import com.google.common.primitives.Ints
import com.google.common.primitives.Chars
import scala.runtime.IntegralProxy
import java.nio.ByteBuffer
import java.nio.ByteOrder

package object math {
  val E = smath.E

  val Pi = smath.Pi

  def random: Double = smath.random

  def sin(x: Double): Double = smath.sin(x)
  def cos(x: Double): Double = smath.cos(x)
  def tan(x: Double): Double = smath.tan(x)
  def asin(x: Double): Double = smath.asin(x)
  def acos(x: Double): Double = smath.acos(x)
  def atan(x: Double): Double = smath.atan(x)

  def toRadians(x: Double): Double = smath.toRadians(x)

  def toDegrees(x: Double): Double = smath.toDegrees(x)

  def exp(x: Double): Double = smath.exp(x)
  def log(x: Double): Double = smath.log(x)
  def sqrt(x: Double): Double = smath.sqrt(x)
  def IEEEremainder(x: Double, y: Double): Double = smath.IEEEremainder(x, y)

  def ceil(x: Double): Double = smath.ceil(x)
  def floor(x: Double): Double = smath.floor(x)

  def rint(x: Double): Double = smath.rint(x)

  def atan2(y: Double, x: Double): Double = smath.atan2(y, x)

  def pow(x: Double, y: Double): Double = smath.pow(x, y)

  def round(x: Float): Int = smath.round(x)
  def round(x: Double): Long = smath.round(x)
  def abs(x: Int): Int = smath.abs(x)
  def abs(x: Long): Long = smath.abs(x)
  def abs(x: Float): Float = smath.abs(x)
  def abs(x: Double): Double = smath.abs(x)

  def max(x: Int, y: Int): Int = smath.max(x, y)
  def max(x: Long, y: Long): Long = smath.max(x, y)
  def max(x: Float, y: Float): Float = smath.max(x, y)
  def max(x: Double, y: Double): Double = smath.max(x, y)

  def min(x: Int, y: Int): Int = smath.min(x, y)
  def min(x: Long, y: Long): Long = smath.min(x, y)
  def min(x: Float, y: Float): Float = smath.min(x, y)
  def min(x: Double, y: Double): Double = smath.min(x, y)

  def signum(x: Double): Double = smath.signum(x)

  def signum(x: Float): Float = smath.signum(x)

  def signum(x: Long): Long = smath.signum(x)

  def signum(x: Int): Int = smath.signum(x)

  def metallic(p: Double, q: Double): Double = {
    (p + pow(p * p + 4 * q, 1.0 / 2)) / 2
  }

  val GoldMean: Double = metallic(1, 1)

  val SilverMean: Double = metallic(2, 1)

  val BronzeMean: Double = metallic(3, 1)

  val CopperMean: Double = metallic(1, 2)

  val NickelMean: Double = metallic(1, 3)

  val TribonacciConstant: Double = {
    (1 + pow(19 + 3 * pow(33, 1.0 / 2), 1.0 / 3) +
      pow(19 - 3 * pow(33, 1.0 / 2), 1.0 / 3)) / 3
  }

  def fib(n: Int): Int = {
    ((pow(GoldMean, n) - pow(-GoldMean, -n)) / pow(5, 1.0 / 2)).toInt
  }

  def lucas(n: Int): Int = {
    (pow(GoldMean, n) + pow(-GoldMean, -n)).toInt
  }

  implicit class RichLong(val self: Long) extends IntegralProxy[Long] {
    import scala.runtime.{RichLong => SRich}
    
    def num = Numeric.LongIsIntegral
    
    def ord = Ordering.Long
    
    def srich: SRich = {
      new SRich(self)
    }
    
    def toBinaryString: String = {
      srich.toBinaryString
    }
    
    def toHexString: String = {
      srich.toHexString
    }
    
    def toOctalString: String = {
      srich.toOctalString
    }
    
    def bswap: Long = {
      val buffer = ByteBuffer.allocate(8)
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      buffer.putLong(self)
      buffer.order(ByteOrder.BIG_ENDIAN)
      buffer.getLong(0)
    }
    
    def toByteChecked: Byte = {
      SignedBytes.checkedCast(self)
    }

    def toShortChecked: Short = {
      Shorts.checkedCast(self)
    }

    def toCharChecked: Char = {
      Chars.checkedCast(self)
    }

    def toIntChecked: Int = {
      Ints.checkedCast(self)
    }
  }
}
