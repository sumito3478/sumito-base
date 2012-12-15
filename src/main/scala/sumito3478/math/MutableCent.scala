package sumito3478.math

class MutableCent(var low: Long, var high: Long) {
  def toCent: Cent = {
    new Cent(low, high)
  }
}

object MutableCent {
  def apply(cent: Cent): MutableCent = {
    new MutableCent(cent.low, cent.high)
  }
}