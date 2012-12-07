package sumito3478.util

package object control {
  def breakable(op: (=> Nothing) => Unit): Unit = {
    op({ return })
  }
}