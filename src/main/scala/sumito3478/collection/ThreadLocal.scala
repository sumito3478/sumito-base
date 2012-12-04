package sumito3478.collection

trait ThreadLocal[+A] {
  def apply: A
}