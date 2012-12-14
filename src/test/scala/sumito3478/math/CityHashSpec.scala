package sumito3478.math

import org.specs2.mutable.SpecificationWithJUnit
import java.nio.ByteBuffer
import java.nio.ByteOrder

class CityHashSpec extends SpecificationWithJUnit {
  "CityHash.cityMurmur" should {
    "..." in {
      var str = "Mis, Mis, Mister, Drill, Driller, I'll do my best, I cant lose!"
      0 until 4 foreach {
        i =>
          str = str + str
      }
      val data = str.getBytes()
      val buffer = ByteBuffer.allocate(data.length)
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      buffer.put(data)
      buffer.position(0)
      println(data.length)
      val result = CityHash.cityHash128WithSeed(buffer, data.length, new Cent(0, 0))
      buffer.position(0)
      val result2 = CityHash.cityMurmur(buffer, data.length, new Cent(0, 0))
      println(f"${result.low}%x; ${result.high}%x")
      println(f"${result2.low}%x; ${result2.high}%x")
      result.low mustEqual 0x431a70f8e128603aL
    }
  }
}