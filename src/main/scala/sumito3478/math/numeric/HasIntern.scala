package sumito3478.math.numeric

trait HasIntern[@specialized(Byte, Short, Int, Long) Repr, @specialized(Byte, Short, Int, Long) Intern] {
  def intern(self: Repr): Intern
}