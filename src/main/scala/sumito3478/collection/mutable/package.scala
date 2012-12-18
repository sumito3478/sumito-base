package sumito3478.collection

import scala.collection.mutable.Map
import scala.collection.mutable.MapLike

package object mutable {
  abstract class AbstractMapLike[@specialized(
  Byte, Short, Char, Long, Float, Double) A, @specialized(
  Byte, Short, Char, Long, Float, Double) B, This <: MapLike[A, B, This] with Map[A, B]] extends Map[A, B] with MapLike[A, B, This] {
    override def empty: This = {
      newBuilder.result
    }
  }
}