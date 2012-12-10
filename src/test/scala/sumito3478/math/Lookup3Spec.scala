package sumito3478.math

import org.specs2.mutable.SpecificationWithJUnit

class Lookup3Spec extends SpecificationWithJUnit {
  def testArray = {
    Array(493827, 2314, 5321453, 6457, 85467854, 634563, 2345325, 4325, 2314, 4325)
  }

  "Lookup3#apply(IndexedSeq)" should {
    "return Jenkins Hash" in {
      Lookup3(testArray) mustEqual 1284356258
    }
  }

  "Lookup3#array(Iterator)" should {
    "return Jenkins Hash" in {
      Lookup3(testArray.iterator, testArray.length) mustEqual 1284356258
    }
  }
}