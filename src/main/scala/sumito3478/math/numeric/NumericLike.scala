package sumito3478.math.numeric

trait NumericLike[@specialized(Byte, Short, Int, Long) Repr, @specialized(Byte, Short, Int, Long) Intern] extends Any {
  def repr: Repr

  def intern(implicit ops: HasIntern[Repr, Intern]): Intern = {
    ops.intern(repr)
  }

  def toBinaryString(implicit ops: ToString[Repr, Intern], fintern: HasIntern[Repr, Intern]): String = {
    ops.toBinaryString(repr)(fintern)
  }

  def toHexString(implicit ops: ToString[Repr, Intern], fintern: HasIntern[Repr, Intern]): String = {
    ops.toHexString(repr)(fintern)
  }

  def toOctalString(implicit ops: ToString[Repr, Intern], fintern: HasIntern[Repr, Intern]): String = {
    ops.toOctalString(repr)(fintern)
  }

  def toIntChecked(implicit ops: ToNumericChecked[Repr, Intern, Int], fops: Numeric[Intern], fintern: HasIntern[Repr, Intern]): Int = {
    ops.cast(repr)(fops, Numeric.IntIsIntegral, fintern)
  }

  def toShortChecked(implicit ops: ToNumericChecked[Repr, Intern, Short], fops: Numeric[Intern], fintern: HasIntern[Repr, Intern]): Short = {
    ops.cast(repr)(fops, Numeric.ShortIsIntegral, fintern)
  }

  def toByteChecked(implicit ops: ToNumericChecked[Repr, Intern, Byte], fops: Numeric[Intern], fintern: HasIntern[Repr, Intern]): Byte = {
    ops.cast(repr)(fops, Numeric.ByteIsIntegral, fintern)
  }
}
  