package sumito3478.math

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import scala.collection.immutable.VectorBuilder
import java.nio.ByteBuffer
import com.google.common.primitives.Longs
import java.nio.ByteOrder
import com.google.common.primitives.UnsignedInts

class CRC32CSpec extends SpecificationWithJUnit {
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

  "CRC32C.crc(CRC32C.init, Iterator[Byte])" should {
    "return CRC32-C hash value" in {
      val crc32c = new com.google.appengine.api.files.Crc32c()
      crc32c.update(testData.toArray, 0, testData.length)
      CRC32C.crc(CRC32C.init, testData.iterator) mustEqual crc32c.getValue().toInt
    }
  }

  "CRC32C.crc(CRC32C.init, Long)" should {
    "return CRC32-C hash value for the little endian binary representation of the long value" in {
      val input = 0x43212421943321L
      val buf = ByteBuffer.allocate(8)
      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.putLong(input)
      val array = buf.array()
      val crc32c = new com.google.appengine.api.files.Crc32c()
      crc32c.update(array, 0, array.length)
      CRC32C.crc(CRC32C.init, array.iterator) mustEqual crc32c.getValue().toInt
    }
  }
}