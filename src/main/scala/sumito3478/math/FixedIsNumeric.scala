package sumito3478.math

trait FixedIsNumeric[Repr, @specialized(Byte, Short, Char, Int, Long) V] extends Numeric[Repr]{
  val base: Int
  
  protected[this] val valueOps: Integral[V]
  
  protected[this] implicit def toV(repr: Repr): V
  
  protected[this] implicit def toRepr(v: V): Repr

  def apply(integer: V, decimal: V): Repr = {
    valueOps.plus(valueOps.times(integer, valueOps.fromInt(1 << base)), decimal)
  }

  def compare(x: Repr, y: Repr): Int = {
    valueOps.compare(x, y)
  }

  def <(x: Repr, y: Repr): Boolean = {
    compare(x, y) < 0
  }

  def <=(x: Repr, y: Repr): Boolean = {
    compare(x, y) <= 0
  }

  def ==(x: Repr, y: Repr): Boolean = {
    compare(x, y) == 0
  }

  def !=(x: Repr, y: Repr): Boolean = {
    !(==(x, y))
  }

  def >(x: Repr, y: Repr): Boolean = {
    !(<(x, y))
  }

  def >=(x: Repr, y: Repr): Boolean = {
    !(<=(x, y))
  }

  def plus(x: Repr, y: Repr): Repr = {
    valueOps.plus(x, y)
  }

  def minus(x: Repr, y: Repr): Repr = {
    valueOps.minus(x, y)
  }

  def times(x: Repr, y: Repr): Repr = {
    valueOps.quot(valueOps.times(x, y), valueOps.fromInt(1 << base))
  }

  def fromInt(x: Int): Repr = {
    valueOps.fromInt(x << base)
  }

  def negate(x: Repr): Repr = {
    valueOps.negate(x)
  }

  def toInt(x: Repr): Int = {
    valueOps.toInt(x) >> base
  }

  def toLong(x: Repr): Long = {
    valueOps.toLong(x) >> base
  }

  def toFloat(x: Repr): Float = {
    valueOps.toFloat(x) / (1 << base)
  }

  def toDouble(x: Repr): Double = {
    valueOps.toDouble(x) / (1 << base)
  }
  
//  def toBigDecimal(x: Repr): BigDecimal = {
//  }
}