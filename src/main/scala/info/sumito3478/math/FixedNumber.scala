package info.sumito3478.math

trait FixedNumber[@specialized(Byte, Short, Char, Int, Long) V] {
  val intern: V
}