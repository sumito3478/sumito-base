package sumito3478.util.control

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import scala.collection.mutable.Queue

class PackageSpec extends SpecificationWithJUnit {
  "breakable" should {
    "be able to break from scope" in {
      val queue = new Queue[Int]
      breakable {
        break =>
          0 to 10 foreach {
            i =>
              if(i < 5) {
                queue.enqueue(i)
              } else {
                break
              }
          }
      }
      val ret = queue.toArray
      ret mustEqual Array(0, 1, 2, 3, 4)
    }
    
    "be able to break from nested breakable scope" in {
      val queue = new Queue[Int]
      breakable {
        break =>
          0 to 10 foreach {
            i =>
              if(i < 5) {
                queue.enqueue(i)
              } else {
                breakable {
                  break2 =>
                    11 to 20 foreach {
                      i =>
                        if(i < 15) {
                          queue.enqueue(i)
                        } else {
                          break // break from outer breakable, not inner
                        }
                    }
                }
              }
          }
      }
      val ret = queue.toArray
      ret mustEqual Array(0, 1, 2, 3, 4, 11, 12, 13, 14)
    }
  }
}