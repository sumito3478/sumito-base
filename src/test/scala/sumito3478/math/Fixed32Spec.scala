package sumito3478.math

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope

class Fixed32Spec extends SpecificationWithJUnit {
  "Fixed32#+" should {
    "add the numbers" in {
      val lhs = Fixed32(5, 253) // 1533
      val rhs = Fixed32(7, 7) // 1799
      (lhs + rhs == Fixed32(13, 4)) must beTrue
      
    }
  }
  "Fixed32#*" should {
    "muptiply the numbers" in {
      val lhs = Fixed32(5, 253) // 1533
      val rhs = Fixed32(7, 7) // 1799
      (lhs * rhs == Fixed32(42, 20)) must beTrue
    }
  }
}