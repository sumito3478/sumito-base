package info.sumito3478.collection

import scala.collection.BufferedIterator
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.Queue
import info.sumito3478.WrapperLike

trait LookAheadIterator[+A] extends BufferedIterator[A] with WrapperLike[Iterator[A]] {
  private[this] val buffer = new Queue[A]

  def lookAhead(n: Int): IndexedSeq[A] = {
    val d = n - buffer.length
    if (d > 0) {
      buffer.enqueue(intern.forceTake(d): _*)
    }
    (new VectorBuilder[A]() ++= buffer.slice(0, n)).result
  }

  def head: A = {
    lookAhead(1).head
  }

  def hasNext: Boolean = {
    lookAhead(1).length > 0
  }

  def next: A = {
    if (buffer.isEmpty) {
      intern.next
    } else {
      buffer.dequeue
    }
  }
}