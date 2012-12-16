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
        builder ++= prims map {
          prim2 =>
            s"""def to${prim2}Checked: ${prim2} = {
              |val ret = intern.to${prim2}
              |val restored = ret.to${prim}
              |require(restored == intern, f"checked cast from ${
              if (List("Byte", "Short", "Int", "Long").contains(prim))
                "0x${intern}%x"
              else
                "${intern}"
            }: ${prim} to ${prim2} failed.")
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
      require(restored == intern, f"checked cast from 0x${intern}%x: Byte to Byte failed.")
      ret
    }
    def toShortChecked: Short = {
      val ret = intern.toShort
      val restored = ret.toByte
      require(restored == intern, f"checked cast from 0x${intern}%x: Byte to Short failed.")
      ret
    }
    def toIntChecked: Int = {
      val ret = intern.toInt
      val restored = ret.toByte
      require(restored == intern, f"checked cast from 0x${intern}%x: Byte to Int failed.")
      ret
    }
    def toLongChecked: Long = {
      val ret = intern.toLong
      val restored = ret.toByte
      require(restored == intern, f"checked cast from 0x${intern}%x: Byte to Long failed.")
      ret
    }
    def toFloatChecked: Float = {
      val ret = intern.toFloat
      val restored = ret.toByte
      require(restored == intern, f"checked cast from 0x${intern}%x: Byte to Float failed.")
      ret
    }
    def toDoubleChecked: Double = {
      val ret = intern.toDouble
      val restored = ret.toByte
      require(restored == intern, f"checked cast from 0x${intern}%x: Byte to Double failed.")
      ret
    }
  }

  implicit class ShortVal(val intern: Short) extends AnyVal {
    def bswap: Short = JShort.reverseBytes(intern)
    def toByteChecked: Byte = {
      val ret = intern.toByte
      val restored = ret.toShort
      require(restored == intern, f"checked cast from 0x${intern}%x: Short to Byte failed.")
      ret
    }
    def toShortChecked: Short = {
      val ret = intern.toShort
      val restored = ret.toShort
      require(restored == intern, f"checked cast from 0x${intern}%x: Short to Short failed.")
      ret
    }
    def toIntChecked: Int = {
      val ret = intern.toInt
      val restored = ret.toShort
      require(restored == intern, f"checked cast from 0x${intern}%x: Short to Int failed.")
      ret
    }
    def toLongChecked: Long = {
      val ret = intern.toLong
      val restored = ret.toShort
      require(restored == intern, f"checked cast from 0x${intern}%x: Short to Long failed.")
      ret
    }
    def toFloatChecked: Float = {
      val ret = intern.toFloat
      val restored = ret.toShort
      require(restored == intern, f"checked cast from 0x${intern}%x: Short to Float failed.")
      ret
    }
    def toDoubleChecked: Double = {
      val ret = intern.toDouble
      val restored = ret.toShort
      require(restored == intern, f"checked cast from 0x${intern}%x: Short to Double failed.")
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
      require(restored == intern, f"checked cast from 0x${intern}%x: Int to Byte failed.")
      ret
    }
    def toShortChecked: Short = {
      val ret = intern.toShort
      val restored = ret.toInt
      require(restored == intern, f"checked cast from 0x${intern}%x: Int to Short failed.")
      ret
    }
    def toIntChecked: Int = {
      val ret = intern.toInt
      val restored = ret.toInt
      require(restored == intern, f"checked cast from 0x${intern}%x: Int to Int failed.")
      ret
    }
    def toLongChecked: Long = {
      val ret = intern.toLong
      val restored = ret.toInt
      require(restored == intern, f"checked cast from 0x${intern}%x: Int to Long failed.")
      ret
    }
    def toFloatChecked: Float = {
      val ret = intern.toFloat
      val restored = ret.toInt
      require(restored == intern, f"checked cast from 0x${intern}%x: Int to Float failed.")
      ret
    }
    def toDoubleChecked: Double = {
      val ret = intern.toDouble
      val restored = ret.toInt
      require(restored == intern, f"checked cast from 0x${intern}%x: Int to Double failed.")
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
      require(restored == intern, f"checked cast from 0x${intern}%x: Long to Byte failed.")
      ret
    }
    def toShortChecked: Short = {
      val ret = intern.toShort
      val restored = ret.toLong
      require(restored == intern, f"checked cast from 0x${intern}%x: Long to Short failed.")
      ret
    }
    def toIntChecked: Int = {
      val ret = intern.toInt
      val restored = ret.toLong
      require(restored == intern, f"checked cast from 0x${intern}%x: Long to Int failed.")
      ret
    }
    def toLongChecked: Long = {
      val ret = intern.toLong
      val restored = ret.toLong
      require(restored == intern, f"checked cast from 0x${intern}%x: Long to Long failed.")
      ret
    }
    def toFloatChecked: Float = {
      val ret = intern.toFloat
      val restored = ret.toLong
      require(restored == intern, f"checked cast from 0x${intern}%x: Long to Float failed.")
      ret
    }
    def toDoubleChecked: Double = {
      val ret = intern.toDouble
      val restored = ret.toLong
      require(restored == intern, f"checked cast from 0x${intern}%x: Long to Double failed.")
      ret
    }
  }

  implicit class FloatVal(val intern: Float) extends AnyVal {
    def toByteChecked: Byte = {
      val ret = intern.toByte
      val restored = ret.toFloat
      require(restored == intern, f"checked cast from ${intern}: Float to Byte failed.")
      ret
    }
    def toShortChecked: Short = {
      val ret = intern.toShort
      val restored = ret.toFloat
      require(restored == intern, f"checked cast from ${intern}: Float to Short failed.")
      ret
    }
    def toIntChecked: Int = {
      val ret = intern.toInt
      val restored = ret.toFloat
      require(restored == intern, f"checked cast from ${intern}: Float to Int failed.")
      ret
    }
    def toLongChecked: Long = {
      val ret = intern.toLong
      val restored = ret.toFloat
      require(restored == intern, f"checked cast from ${intern}: Float to Long failed.")
      ret
    }
    def toFloatChecked: Float = {
      val ret = intern.toFloat
      val restored = ret.toFloat
      require(restored == intern, f"checked cast from ${intern}: Float to Float failed.")
      ret
    }
    def toDoubleChecked: Double = {
      val ret = intern.toDouble
      val restored = ret.toFloat
      require(restored == intern, f"checked cast from ${intern}: Float to Double failed.")
      ret
    }
  }

  implicit class DoubleVal(val intern: Double) extends AnyVal {
    def toByteChecked: Byte = {
      val ret = intern.toByte
      val restored = ret.toDouble
      require(restored == intern, f"checked cast from ${intern}: Double to Byte failed.")
      ret
    }
    def toShortChecked: Short = {
      val ret = intern.toShort
      val restored = ret.toDouble
      require(restored == intern, f"checked cast from ${intern}: Double to Short failed.")
      ret
    }
    def toIntChecked: Int = {
      val ret = intern.toInt
      val restored = ret.toDouble
      require(restored == intern, f"checked cast from ${intern}: Double to Int failed.")
      ret
    }
    def toLongChecked: Long = {
      val ret = intern.toLong
      val restored = ret.toDouble
      require(restored == intern, f"checked cast from ${intern}: Double to Long failed.")
      ret
    }
    def toFloatChecked: Float = {
      val ret = intern.toFloat
      val restored = ret.toDouble
      require(restored == intern, f"checked cast from ${intern}: Double to Float failed.")
      ret
    }
    def toDoubleChecked: Double = {
      val ret = intern.toDouble
      val restored = ret.toDouble
      require(restored == intern, f"checked cast from ${intern}: Double to Double failed.")
      ret
    }
  }
}