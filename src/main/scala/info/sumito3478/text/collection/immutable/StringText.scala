package info.sumito3478.text.collection.immutable

import info.sumito3478.text.collection.IndexedSeqTextLike
import info.sumito3478.text.collection.mutable.StringBuilder
import scala.collection.mutable.Builder

class StringText(val intern: String) extends IndexedSeq[Char] with IndexedSeqTextLike[StringText] {
  override def newBuilder: StringBuilder = new StringBuilder
  
  def apply(idx: Int): Char = intern charAt idx

  def length: Int = intern.length
}

object StringText {
  def newBuilder: StringBuilder = new StringBuilder
}
