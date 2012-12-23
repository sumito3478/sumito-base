package info.sumito3478.text.collection

import scala.collection.IndexedSeqOptimized

/**
 * A template trait for indexed sequences representing text data.
 */
trait IndexedSeqTextLike[+Repr <: IndexedSeqTextLike[Repr]] extends IndexedSeqOptimized[Char, Repr] with CharSequence {
  /**
   * implementation of CharSequence#charAt.
   */
  def charAt(idx: Int): Char = apply(idx)

  /**
   * implementation of CharSequence#subSequence
   */
  def subSequence(start: Int, end: Int): CharSequence = {
    slice(start, end)
  }

  /**
   * Convert text represented by this indexed sequence to java.lang.String.
   */
  override def toString(): String = {
    (new scala.collection.mutable.StringBuilder ++= this).result
  }
}