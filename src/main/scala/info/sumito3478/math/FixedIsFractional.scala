package info.sumito3478.math

trait FixedIsFractional[Repr, @specialized(Byte, Short, Char, Int, Long) V] extends FixedIsNumeric[Repr, V] with Fractional[Repr]{
  def div(x: Repr, y: Repr): Repr = {
    valueOps.quot(valueOps.times(x, valueOps.fromInt(1 << base)), y)
  }
}