package info.sumito3478.math

class Cent(val low: Long, val high: Long) {
  override def equals(that: Any): Boolean = that match {
    case that: Cent => low == that.low && high == that.high
    case _ => false
  }
}