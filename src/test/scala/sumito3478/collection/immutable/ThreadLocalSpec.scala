package sumito3478.collection.immutable

import org.specs2.mutable.SpecificationWithJUnit
import org.specs2.specification.Scope
import scala.collection.mutable.Queue

class ThreadLocalSpec extends SpecificationWithJUnit {
  "ThreadLocal#apply" should {
    "the instance of the thread local variable" in {
      (ThreadLocal[Int] {
        () =>
          0
      })() mustEqual 0
    }
    
    "the same instance in one thread" in {
      val local = ThreadLocal[Queue[Int]] {
        () =>
          Queue(0, 1, 2, 3, 4)
      }
      val queue = local()
      queue.enqueue(5, 6, 7, 8, 9)
      local() mustEqual Queue(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    }
    
    "instances of variables independent per thread" in {
      val local = ThreadLocal[Queue[Int]] {
        () => 
          new Queue[Int]
      }
      val queue = local()
      queue.enqueue(0, 1, 2, 3)
      val thread = new Thread {
        override def run: Unit = {
          val queue = local()
          queue.enqueue(0, 1, 2, 3, 4)
        }
      }
      thread.start
      thread.join
      queue mustEqual Queue(0, 1, 2, 3)
    }
  }
}