package sumito3478.collection.mutable

import org.specs2.mutable.SpecificationWithJUnit
import scala.collection.mutable.Map

class UnorderedMapSpec extends SpecificationWithJUnit {
  def sum[A](xs: Iterable[Int]) = (0 /: xs)((x, y) => x + y)
  
  "(unorderedMap: Map)#+" should {
    "return Map that is sum of lhs and rhs" in {
      var s: Map[Int, Int] = UnorderedMap[Int, Int]()
      s = s + (2 -> 2)
      s = s + (3 ->3, 4000 -> 4000, 10000 -> 10000)
      sum(s map(_._2)) mustEqual 14005
    }
  }
  
  "(unorderedMap: Map)#++" should {
    "return Map that is sum of lhs and rhs" in {
      var s: Map[Int, Int] = UnorderedMap[Int, Int]()
      s = s ++ (0 until 5000 map (x => x * 2 -> x * 2))
      sum(s map(_._2)) mustEqual 0x17d64b8
    }
  }
  
  "(unorderedMap: Map)#get" should {
    "return the Some(value) if exists, or None" in {
      var s: Map[Int, Int] = UnorderedMap[Int, Int]()
      s = s ++ ((0 until 5000) map (x => x * 2 -> x * 2))
      var x = 0
      0 to 10000 foreach {
        i =>
          s get i match {
            case Some(v) => x += i
            case None =>
          }
      }
      x mustEqual 0x17d64b8
    }
  }
}