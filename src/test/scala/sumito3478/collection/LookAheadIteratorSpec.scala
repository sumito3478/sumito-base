package sumito3478.collection

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope

class LookAheadIteratorSpec extends SpecificationWithJUnit {
  "LookAheadIterator#lookAhead" should {
    "returns same result when called twice in a row" in {
      val la = List(0, 1, 2, 3, 4).iterator.lookAhead
      la.lookAhead(3) mustEqual la.lookAhead(3)
    }
    
    "returns result of which length equals to the argument, if available" in {
      val la = List(0, 1, 2, 3, 4).iterator.lookAhead
      la.lookAhead(3).length mustEqual 3
    }
    
    "returns result of which length equals to the size of avaiable elements, if it is smaller than the argument" in {
      val la = List(0, 1).iterator.lookAhead
      la.lookAhead(3).length mustEqual 2
    }
    
    "returns empty result if no element avaiable" in {
      val la = List().iterator.lookAhead
      la.lookAhead(3) mustEqual Vector()
    }
  }
}