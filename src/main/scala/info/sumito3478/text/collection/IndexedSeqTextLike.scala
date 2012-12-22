package info.sumito3478.text.collection

import scala.collection.IndexedSeqOptimized

trait IndexedSeqTextLike[+Repr <: IndexedSeqTextLike[Repr]] extends IndexedSeqOptimized[Char, Repr] with CharSequence {
  def charAt(idx: Int): Char = apply(idx)

  def subSequence(start: Int, end: Int): CharSequence = {
    slice(start, end)
  }

  override def toString(): String = {
    (new scala.collection.mutable.StringBuilder ++= this).result
  }
}