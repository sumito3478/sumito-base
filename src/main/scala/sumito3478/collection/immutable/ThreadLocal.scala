package sumito3478.collection.immutable

import java.lang.{ ThreadLocal => JThreadLocal}

trait ThreadLocal[+A] extends _root_.sumito3478.collection.ThreadLocal[A] {
  protected[this] val intern: JThreadLocal[A]
  
  def apply(): A = {
    intern.get
  }
}

object ThreadLocal {
  def apply[A](f: () => A): ThreadLocal[A] = {
    new ThreadLocal[A] {
      val intern = new JThreadLocal[A] {
        override def initialValue: A = {
          f()
        }
      }
    }
  }
}
