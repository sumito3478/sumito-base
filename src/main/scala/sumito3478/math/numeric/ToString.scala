package sumito3478.math.numeric

import java.lang.{Long => JLong}
import java.lang.{Integer => JInt}
import java.lang.{Short => JShort}
import java.lang.{Byte => JByte}
import java.lang.{Float => JFloat}
import java.lang.{Double => JDouble}

trait ToString[@specialized(Byte, Short, Int, Long) From, @specialized(Byte, Short, Int, Long) Intern] {
  def toBinaryString(x: From)(implicit fintern: HasIntern[From, Intern]): String

  def toHexString(x: From)(implicit fintern: HasIntern[From, Intern]): String

  def toOctalString(x: From)(implicit fintern: HasIntern[From, Intern]): String
}

object ToString {
  implicit object LongToString extends ToString[LongVal, Long] {
    def toBinaryString(x: LongVal)(implicit fintern: HasIntern[LongVal, Long]): String = JLong.toBinaryString(fintern.intern(x))
    
    def toHexString(x: LongVal)(implicit fintern: HasIntern[LongVal, Long]): String = JLong.toHexString(fintern.intern(x))
    
    def toOctalString(x: LongVal)(implicit fintern: HasIntern[LongVal, Long]): String = JLong.toOctalString(fintern.intern(x))
  }
  
  implicit object IntToString extends ToString[IntVal, Int] {
    def toBinaryString(x: IntVal)(implicit fintern: HasIntern[IntVal, Int]): String = JInt.toBinaryString(fintern.intern(x))
    
    def toHexString(x: IntVal)(implicit fintern: HasIntern[IntVal, Int]): String = JInt.toHexString(fintern.intern(x))
    
    def toOctalString(x: IntVal)(implicit fintern: HasIntern[IntVal, Int]): String = JInt.toOctalString(fintern.intern(x))
  }
}