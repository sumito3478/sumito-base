package sumito3478.math

import org.specs2.mutable.SpecificationWithJUnit
import java.nio.ByteBuffer
import java.nio.ByteOrder
import sumito3478.ByteBufferPointer

class CityHashSpec extends SpecificationWithJUnit {
  /*
   * the expected values are computed by the original CityHash *v1.1*
   * implementation in C++.
   */
  
  val s = {
    var str = "Mis, Mis, Mister, Drill, Driller, I'll do my best, I cant lose!"
    0 until 4 foreach {
      i =>
        str = str + " " + str
    }
    val data = str.getBytes()
    val buffer = ByteBuffer.allocate(data.length)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.put(data)
    buffer.position(0)
    new ByteBufferPointer(buffer, 0)
  }
  
  "CityHash.cityHash64WithSeed" should {
    val seed = 0xdeadbeefcafebabeL
    
    "return expected value for 8-byte (0 < len =< 16) input" in {
      CityHash.cityHash64WithSeed(s, 8, seed) mustEqual 0x3e25b800e2ef42d8L
    }
    
    "return expected value for 23-byte (16 < len =< 32) input" in {
      CityHash.cityHash64WithSeed(s, 23, seed) mustEqual 0x016a7d2b323985e5L
    }
    
    "return expected value for 63-byte (32 < len =< 64) input" in {
      CityHash.cityHash64WithSeed(s, 63, seed) mustEqual 0x460f91c680702646L
    }
    
    "return expected value for 126-byte (len > 64) input" in {
      CityHash.cityHash64WithSeed(s, 126, seed) mustEqual 0x9c64e03a5baf46f7L
    }
  }
  
  "CityHash.cityHash128WithSeed" should {
    val seed = new Cent(0xdeadbeefcafebabeL, 0xfacefeedfee1deadL)
    
    "return expected value for 126-byte (len < 900) input" in {
      CityHash.cityHash128WithSeed(s, 126, seed) mustEqual new Cent(0x64b62e405a3f61ffL, 0x0308db7b4e08a771L)
    }
    
    "return expected value for 1000-byte (len > 900) input" in {
      CityHash.cityHash128WithSeed(s, 1000, seed) mustEqual new Cent(0xcca027d3b1572ff0L, 0xdbbd8f4445776082L)
    }
  }
}