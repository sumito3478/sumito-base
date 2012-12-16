package sumito3478.math

import scala.runtime.IntegralProxy

import scala.{math => smath}
import smath.Numeric

import com.google.common.primitives.SignedBytes
import com.google.common.primitives.Shorts
import com.google.common.primitives.Chars
import com.google.common.primitives.Ints

import java.lang.{Long => JLong}
import java.lang.{Integer => JInt}
import java.lang.{Short => JShort}
import java.lang.{Byte => JByte}
import java.lang.{Float => JFloat}
import java.lang.{Double => JDouble}

package object numeric {
  implicit class LongVal(val _intern: Long) extends AnyVal with NumericLike[LongVal, Long]{
    def repr = this
  }
  
  implicit object LongValHasIntern extends HasIntern[LongVal, Long] {
    def intern(self: LongVal): Long = {
      self._intern
    }
  }
  
  implicit class IntVal(val _intern: Int) extends AnyVal with NumericLike[IntVal, Int]{
    def repr = this
  }
  
  implicit class ShortVal(val _intern: Short) extends AnyVal with NumericLike[ShortVal, Short]{
    def repr = this
  }
  
  implicit class ByteVal(val intern: Byte) extends AnyVal with NumericLike[ByteVal, Byte]{
    def repr = this
  }
  
  class BooleanVal(val intern: Boolean) extends AnyVal {
    
  }
  
  class FloatVal(val intern: Float) extends AnyVal {
    def round: Int = smath.round(intern)
    
    def ceil: Float = smath.ceil(intern).toFloat
    
    def floor: Float = smath.floor(intern).toFloat
    
    def toRadians: Float = smath.toRadians(intern).toFloat
    
    def toDegrees: Float = smath.toDegrees(intern).toFloat
    
    def isInfinity: Boolean = JFloat.isInfinite(intern)
    
    def isPosInfinity: Boolean = isInfinity && intern > 0.0f
    
    def isNegInfinity: Boolean = isInfinity && intern < 0.0f
  }
  
  class DoubleVal(val intern: Double) extends AnyVal {
    def round: Long = smath.round(intern)
    
    def ceil: Double = smath.ceil(intern)
    
    def floor: Double = smath.floor(intern)
    
    def toRadians: Double = smath.toRadians(intern)
    
    def toDegrees: Double = smath.toDegrees(intern)
    
    def isInfinity: Boolean = JDouble.isInfinite(intern)
    
    def isPosInfinity: Boolean = isInfinity && intern > 0.0
    
    def isNegInfinity: Boolean = isInfinity && intern < 0.0
  }
}