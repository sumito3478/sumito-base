package sumito3478.math

import scala.runtime.IntegralProxy
import scala.{ math => smath }
import smath.Numeric
import com.google.common.primitives.SignedBytes
import com.google.common.primitives.Shorts
import com.google.common.primitives.Chars
import com.google.common.primitives.Ints
import java.lang.{ Long => JLong }
import java.lang.{ Integer => JInt }
import java.lang.{ Short => JShort }
import java.lang.{ Byte => JByte }
import java.lang.{ Float => JFloat }
import java.lang.{ Double => JDouble }
import scala.collection.immutable.VectorBuilder

package object numeric {
  val prims = List("Byte", "Short", "Int", "Long", "Float", "Double")

  def generatePrimitiveSource: String = {
    prims map {
      prim =>
        val builder = new VectorBuilder[String]
        builder += s"implicit class ${prim}Val(val intern: ${prim}) extends AnyVal {"
        if (List("Short", "Int", "Long").contains(prim)) {
          builder += s"def bswap: ${prim} = J${prim}.reverseBytes(intern)"
        }
        if (List("Int", "Long").contains(prim)) {
          builder += s"def toBinaryString: String = J${prim}.toBinaryString(intern)"
          builder += s"def toHexString: String = J${prim}.toHexString(intern)"
          builder += s"def toOctalString: String = J${prim}.toOctalString(intern)"
        }
        prims foreach {
          prim2 =>
            builder += s"""def to${prim2}Checked: ${prim2} = {
              |val ret = intern.to${prim2}
              |val restored = ret.to${prim}
              |require(restored == intern, s"checked cast from $${intern}: ${prim} to ${prims} failed.)")
              |ret
              |}""".stripMargin
        }
        builder += s"}"
        builder.result mkString "\n"
    } mkString "\n\n"
  }
  implicit class ByteVal(val intern: Byte) extends AnyVal {
    def toByteChecked: Byte = {
      val ret = intern.toByte
      val restored = ret.toByte
      require(restored == intern, s"checked cast from ${intern}: Byte to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toShortChecked: Short = {
      val ret = intern.toShort
      val restored = ret.toByte
      require(restored == intern, s"checked cast from ${intern}: Byte to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toIntChecked: Int = {
      val ret = intern.toInt
      val restored = ret.toByte
      require(restored == intern, s"checked cast from ${intern}: Byte to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toLongChecked: Long = {
      val ret = intern.toLong
      val restored = ret.toByte
      require(restored == intern, s"checked cast from ${intern}: Byte to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toFloatChecked: Float = {
      val ret = intern.toFloat
      val restored = ret.toByte
      require(restored == intern, s"checked cast from ${intern}: Byte to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toDoubleChecked: Double = {
      val ret = intern.toDouble
      val restored = ret.toByte
      require(restored == intern, s"checked cast from ${intern}: Byte to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
  }

  implicit class ShortVal(val intern: Short) extends AnyVal {
    def bswap: Short = JShort.reverseBytes(intern)
    def toByteChecked: Byte = {
      val ret = intern.toByte
      val restored = ret.toShort
      require(restored == intern, s"checked cast from ${intern}: Short to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toShortChecked: Short = {
      val ret = intern.toShort
      val restored = ret.toShort
      require(restored == intern, s"checked cast from ${intern}: Short to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toIntChecked: Int = {
      val ret = intern.toInt
      val restored = ret.toShort
      require(restored == intern, s"checked cast from ${intern}: Short to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toLongChecked: Long = {
      val ret = intern.toLong
      val restored = ret.toShort
      require(restored == intern, s"checked cast from ${intern}: Short to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toFloatChecked: Float = {
      val ret = intern.toFloat
      val restored = ret.toShort
      require(restored == intern, s"checked cast from ${intern}: Short to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toDoubleChecked: Double = {
      val ret = intern.toDouble
      val restored = ret.toShort
      require(restored == intern, s"checked cast from ${intern}: Short to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
  }

  implicit class IntVal(val intern: Int) extends AnyVal {
    def bswap: Int = JInt.reverseBytes(intern)
    def toBinaryString: String = JInt.toBinaryString(intern)
    def toHexString: String = JInt.toHexString(intern)
    def toOctalString: String = JInt.toOctalString(intern)
    def toByteChecked: Byte = {
      val ret = intern.toByte
      val restored = ret.toInt
      require(restored == intern, s"checked cast from ${intern}: Int to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toShortChecked: Short = {
      val ret = intern.toShort
      val restored = ret.toInt
      require(restored == intern, s"checked cast from ${intern}: Int to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toIntChecked: Int = {
      val ret = intern.toInt
      val restored = ret.toInt
      require(restored == intern, s"checked cast from ${intern}: Int to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toLongChecked: Long = {
      val ret = intern.toLong
      val restored = ret.toInt
      require(restored == intern, s"checked cast from ${intern}: Int to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toFloatChecked: Float = {
      val ret = intern.toFloat
      val restored = ret.toInt
      require(restored == intern, s"checked cast from ${intern}: Int to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toDoubleChecked: Double = {
      val ret = intern.toDouble
      val restored = ret.toInt
      require(restored == intern, s"checked cast from ${intern}: Int to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
  }

  implicit class LongVal(val intern: Long) extends AnyVal {
    def bswap: Long = JLong.reverseBytes(intern)
    def toBinaryString: String = JLong.toBinaryString(intern)
    def toHexString: String = JLong.toHexString(intern)
    def toOctalString: String = JLong.toOctalString(intern)
    def toByteChecked: Byte = {
      val ret = intern.toByte
      val restored = ret.toLong
      require(restored == intern, s"checked cast from ${intern}: Long to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toShortChecked: Short = {
      val ret = intern.toShort
      val restored = ret.toLong
      require(restored == intern, s"checked cast from ${intern}: Long to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toIntChecked: Int = {
      val ret = intern.toInt
      val restored = ret.toLong
      require(restored == intern, s"checked cast from ${intern}: Long to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toLongChecked: Long = {
      val ret = intern.toLong
      val restored = ret.toLong
      require(restored == intern, s"checked cast from ${intern}: Long to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toFloatChecked: Float = {
      val ret = intern.toFloat
      val restored = ret.toLong
      require(restored == intern, s"checked cast from ${intern}: Long to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toDoubleChecked: Double = {
      val ret = intern.toDouble
      val restored = ret.toLong
      require(restored == intern, s"checked cast from ${intern}: Long to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
  }

  implicit class FloatVal(val intern: Float) extends AnyVal {
    def toByteChecked: Byte = {
      val ret = intern.toByte
      val restored = ret.toFloat
      require(restored == intern, s"checked cast from ${intern}: Float to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toShortChecked: Short = {
      val ret = intern.toShort
      val restored = ret.toFloat
      require(restored == intern, s"checked cast from ${intern}: Float to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toIntChecked: Int = {
      val ret = intern.toInt
      val restored = ret.toFloat
      require(restored == intern, s"checked cast from ${intern}: Float to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toLongChecked: Long = {
      val ret = intern.toLong
      val restored = ret.toFloat
      require(restored == intern, s"checked cast from ${intern}: Float to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toFloatChecked: Float = {
      val ret = intern.toFloat
      val restored = ret.toFloat
      require(restored == intern, s"checked cast from ${intern}: Float to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toDoubleChecked: Double = {
      val ret = intern.toDouble
      val restored = ret.toFloat
      require(restored == intern, s"checked cast from ${intern}: Float to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
  }

  implicit class DoubleVal(val intern: Double) extends AnyVal {
    def toByteChecked: Byte = {
      val ret = intern.toByte
      val restored = ret.toDouble
      require(restored == intern, s"checked cast from ${intern}: Double to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toShortChecked: Short = {
      val ret = intern.toShort
      val restored = ret.toDouble
      require(restored == intern, s"checked cast from ${intern}: Double to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toIntChecked: Int = {
      val ret = intern.toInt
      val restored = ret.toDouble
      require(restored == intern, s"checked cast from ${intern}: Double to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toLongChecked: Long = {
      val ret = intern.toLong
      val restored = ret.toDouble
      require(restored == intern, s"checked cast from ${intern}: Double to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toFloatChecked: Float = {
      val ret = intern.toFloat
      val restored = ret.toDouble
      require(restored == intern, s"checked cast from ${intern}: Double to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
    def toDoubleChecked: Double = {
      val ret = intern.toDouble
      val restored = ret.toDouble
      require(restored == intern, s"checked cast from ${intern}: Double to List(Byte, Short, Int, Long, Float, Double) failed.)")
      ret
    }
  }
}