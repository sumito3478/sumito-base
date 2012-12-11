package sumito3478.math

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import scala.collection.immutable.VectorBuilder
import java.nio.ByteBuffer
import com.google.common.primitives.Longs
import java.nio.ByteOrder
import com.google.common.primitives.UnsignedInts

class CRC64Spec extends SpecificationWithJUnit {
  val testData: IndexedSeq[Byte] = {
    val builder = new VectorBuilder[Byte]
    builder ++= Array[Int](
      0x01, 0xC0, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00,
      0x01, 0xFE, 0x60, 0xAC,
      0x00, 0x00, 0x00, 0x08,
      0x00, 0x00, 0x00, 0x04,
      0x00, 0x00, 0x00, 0x09,
      0x25, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00).map(_.toByte)
    builder.result
  }

  "CRC64.crc(CRC64.init, Iterator[Byte])" should {
    "return CRC64 hash value" in {
      CRC64.crc(CRC64.init, testData.iterator) mustEqual 0xd180735f5e3e58afL
    }
  }

  "CRC64.crc(CRC64.init, Long)" should {
    "return CRC64-C hash value for the little endian binary representation of the long value" in {
      val input = 0x43212421943321L
      val buf = ByteBuffer.allocate(8)
      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.putLong(input)
      val array = buf.array()
      CRC64.crc(CRC64.init, array.iterator) mustEqual CRC64.crc(CRC64.init, input)
    }
  }
}