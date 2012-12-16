package sumito3478.math.numeric

trait ToNumericChecked[@specialized(Byte, Short, Int, Long) From, @specialized(Byte, Short, Int, Long) Intern, @specialized(Byte, Short, Int, Long) To] {
  def cast(x: From)(implicit fops: Numeric[Intern], tops: Numeric[To], fintern: HasIntern[From, Intern]): To
}

object ToNumericChecked {
  trait ToIntChecked[@specialized(Byte, Short, Int, Long) From, @specialized(Byte, Short, Int, Long) Intern] extends ToNumericChecked[From, Intern, Int] {
    def cast(x: From)(implicit fops: Numeric[Intern], tops: Numeric[Int], fintern: HasIntern[From, Intern]): Int = {
      val ret = fintern.intern(x).asInstanceOf[Int]
      require(ret.asInstanceOf[Long] == fintern.intern(x).asInstanceOf[Long], "checked cast failed - use debugger for detail.")
      ret
    }
  }
  
  implicit object LongToIntChecked extends ToIntChecked[LongVal, Long]
  
  trait ToNumericCheckedViaInt[@specialized(Byte, Short, Int, Long) From, @specialized(Byte, Short, Int, Long) Intern, @specialized(Byte, Short, Int, Long) To] extends ToNumericChecked[From, Intern, To] {
    object icast extends ToIntChecked[From, Intern]
    
    def cast(x: From)(implicit fops: Numeric[Intern], tops: Numeric[To], fintern: HasIntern[From, Intern]): To = {
      val tmp0 = icast.cast(x)(fops, Numeric.IntIsIntegral, fintern)
//      val ret = tops.fromInt(tmp0)
//      val tmp1 = tops.toInt(ret)
//      val tmp2 = tops.fromInt(tmp1)
      val ret = tmp0.asInstanceOf[To]
      val tmp1 = ret.asInstanceOf[Int]
      val tmp2 = tmp1.asInstanceOf[To]
      require(ret.asInstanceOf[Int] == tmp0, "checked cast failed - use debugger for detail.")
      ret
    }
  }
  
  implicit object LongToShortChecked extends ToNumericCheckedViaInt[LongVal, Long, Short]
  
  implicit object LongToByteChecked extends ToNumericCheckedViaInt[LongVal, Long, Byte]
  
  implicit object IntToShortChecked extends ToNumericCheckedViaInt[IntVal, Int, Short]
  
  implicit object IntToByteChecked extends ToNumericCheckedViaInt[IntVal, Int, Byte]
  
  implicit object ShortToByteChecked extends ToNumericCheckedViaInt[ShortVal, Short, Byte]
}