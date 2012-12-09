package sumito3478.collection

import java.lang.{ ThreadLocal => JThreadLocal }
import sumito3478.WrapperLike

trait ThreadLocal[+A] {
  def apply(): A
}

object ThreadLocal {
  private[this] class impl[A](
    val intern: JThreadLocal[A]) extends ThreadLocal[A]
    with WrapperLike[JThreadLocal[A]] {
    def apply(): A = {
      intern.get
    }
  }

  def apply[A](f: () => A): ThreadLocal[A] = {
    new impl[A](
      new JThreadLocal[A] {
        override def initialValue: A = {
          f()
        }
      })
  }
}
