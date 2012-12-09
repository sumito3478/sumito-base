package sumito3478.math

import scala.runtime.FractionalProxy

abstract class FixedNumberLike[Repr <: FixedNumberLike[Repr, V], @specialized(Byte, Short, Char, Int, Long) V] extends FractionalProxy[Repr] with FixedNumber[V] {
  def ops = {
    num.mkNumericOps(self)
  }
  
  def *(x: Repr): Repr = {
    ops * x
  }
  
  def +(x: Repr): Repr = {
    ops + x
  }
  
  def -(x: Repr): Repr = {
    ops - x
  }
  
  def /(x: Repr): Repr = {
    ops / x
  }
  
  def unary_-(): Repr = {
    - ops
  }
  
  def equals(x: Repr): Boolean = {
    this.intern == x.intern
  }
  
  def ==(x: Repr): Boolean = {
    equals(x)
  }
}