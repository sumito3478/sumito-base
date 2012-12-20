package info.sumito3478.math

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import scala.collection.immutable.VectorBuilder
import info.sumito3478.math.GeneralizedCRC32;

import java.nio.ByteBuffer
import com.google.common.primitives.Longs
import java.nio.ByteOrder
import com.google.common.primitives.UnsignedInts

class GeneralizedCRC32Spec extends SpecificationWithJUnit {
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

  "GeneralizedCRC32.crc(GeneralizedCRC32.init, Iterator[Byte])" should {
    "returns generalized CRC hash value" in {
      GeneralizedCRC32.crc(GeneralizedCRC32.init, testData.iterator) mustEqual 0xee2bb609
    }
  }

  "GeneralizedCRC32C.crc(GeneralizedCRC32C.init, Long)" should {
    "return generalized CRC hash value for the little endian binary representation of the long value" in {
      val input = 0x43212421943321L
      val buf = ByteBuffer.allocate(8)
      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.putLong(input)
      val array = buf.array()
      GeneralizedCRC32.crc(GeneralizedCRC32.init, input) mustEqual GeneralizedCRC32.crc(GeneralizedCRC32.init, array.iterator)
    }
  }
}