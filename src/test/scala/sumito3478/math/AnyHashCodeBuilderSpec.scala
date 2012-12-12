package sumito3478.math

import org.specs2.mutable.SpecificationWithJUnit
import sumito3478.AnyHashCodeBuilder

class AnyHashCodeBuilderSpec extends SpecificationWithJUnit {
  "AnyHashCodeBuilderSpec" should {
    class A(val a: Int, val b: Int, val c: Byte, val d: Long, val e: Long) {
      override def hashCode: Int = {
        val builder = new AnyHashCodeBuilder
        (builder ++= (a, b) ++= c ++= (d, e)).result
      }
    }
    
    "generate same result for the same input" in {
      val a0 = new A(4321987, 349834, 30, 0xdeadbeefdeadbeefL, 0xabcddeadbeefabcdL)
      val a1 = new A(4321987, 349834, 30, 0xdeadbeefdeadbeefL, 0xabcddeadbeefabcdL)
      a0.hashCode mustEqual a1.hashCode
    }
  }
}