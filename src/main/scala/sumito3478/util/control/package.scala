package sumito3478.util.control

object Breaks {
  def breakable(op: (=> Nothing) => Unit): Unit = {
    op({ return })
  }
}