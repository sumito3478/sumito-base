package info.sumito3478.math

trait FixedAsIfIntegral[Repr, @specialized(Byte, Short, Char, Int, Long) V] extends FixedIsNumeric[Repr, V] with Integral[Repr]{
  def quot(x: Repr, y: Repr): Repr = {
    valueOps.quot(x, y)
  }
  
  def rem(x: Repr, y: Repr): Repr = {
    valueOps.quot(valueOps.rem(x, y), valueOps.fromInt(1 << base))
  }
}