package sumito3478.collection

import scala.collection.mutable.Map
import scala.collection.mutable.MapLike
import scala.collection.generic.MutableMapFactory
import sumito3478.Dummy0

package object mutable {
  abstract class AbstractMapLike[@specialized(
  Byte, Short, Char, Long, Float, Double) A, @specialized(
  Byte, Short, Char, Long, Float, Double) B, This <: MapLike[A, B, This] with Map[A, B]] extends Map[A, B] with MapLike[A, B, This] {
    def empty[T >: This](implicit dummy0: Dummy0): This
    
    override def empty: This = {
      empty[This]
    }
  }
}