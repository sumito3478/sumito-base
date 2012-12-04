package sumito3478.collection

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope

class RichIteratorSpec extends SpecificationWithJUnit {
  "RichIterator#forceTake" should {
    "takes elements that the internal iterator produces" in {
      import sumito3478.collection.Riches.RIterator
      val ret = List(0, 1, 2, 3, 4).iterator.forceTake(3)
      ret mustEqual Vector(0, 1, 2)
    }
    
    "returns a vector of which length equals to the argument, if available" in {
      import sumito3478.collection.Riches.RIterator
      val ret = List(0, 1, 2, 3, 4).iterator.forceTake(3)
      ret.length mustEqual 3
    }
    
    "returns vector of which length equals to the number of available elements, if it is smaller than the argument" in {
      import sumito3478.collection.Riches.RIterator
      val ret = List(0, 1).iterator.forceTake(2)
      ret.length mustEqual 2
    }
    
    "returns empty vector if no element avaiable" in {
      import sumito3478.collection.Riches.RIterator
      val ret = List().iterator.forceTake(2)
      ret mustEqual Vector()
    }
  }
}