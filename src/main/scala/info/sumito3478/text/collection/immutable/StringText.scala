package info.sumito3478.text.collection.immutable

import info.sumito3478.text.collection.IndexedSeqTextLike
import info.sumito3478.text.collection.mutable.StringTextBuilder
import scala.collection.mutable.Builder

/**
 * A IndexedSeqTextLike implementation backed by java.lang.String.
 */
class StringText(val intern: String) extends IndexedSeq[Char] with IndexedSeqTextLike[StringText] {
  override def newBuilder: StringTextBuilder = new StringTextBuilder
  
  def apply(idx: Int): Char = intern charAt idx

  def length: Int = intern.length
}

object StringText {
  def newBuilder: StringTextBuilder = new StringTextBuilder
}
