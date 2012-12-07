package sumito3478.collection

import scala.collection.immutable.VectorBuilder
import sumito3478.util.control.Breaks.breakable
import sumito3478.WrapperLike

trait RichIterator[+A] extends WrapperLike[Iterator[A]]{
  def forceTake(n: Int): IndexedSeq[A] = {
    val builder = new VectorBuilder[A]
    breakable {
      break =>
        0 until n foreach {
          _ =>
            if (intern.hasNext) {
              builder += intern.next
            } else {
              break
            }
        }
    }
    builder.result
  }

  def forceDrop(n: Int): this.type = {
    breakable {
      break =>
        0 until n foreach {
          _ =>
            if (intern.hasNext) {
              intern.next
            } else {
              break
            }
        }
    }
    this
  }

  def lookAhead: LookAheadIterator[A] = {
    val i = intern
    new LookAheadIterator[A] {
      val intern: Iterator[A] = i
    }
  }
}