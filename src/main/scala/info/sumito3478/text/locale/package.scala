package info.sumito3478.text

import com.ibm.icu.text.BreakIterator
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.Queue
import info.sumito3478.text.collection.immutable.StringText
import info.sumito3478.text.collection.mutable.StringTextBuilder
import info.sumito3478.collection.RichIterator

package object locale {

  implicit class BreakIteratorVal(val intern: BreakIterator) extends AnyVal {
    private[this] def readAll(source: StringText): IndexedSeq[StringText] = {
      intern.setText(source.toString)
      val first = intern.first
      val builder = new VectorBuilder[Int]
      builder ++= (Iterator.continually(intern.next).
        takeWhile(_ != BreakIterator.DONE))
      val retBuilder = new VectorBuilder[StringText]
      builder.result.foldLeft(first)((first, end) => {
        retBuilder += new StringText(source.toString.substring(first, end))
        end
      })
      retBuilder.result
    }

    private[this] def readToSecondLast(source: StringText): (IndexedSeq[StringText], Int) = {
      intern.setText(source.toString)
      val first = intern.first
      val last = intern.last
      val secondLast = intern.previous
      if (secondLast == first) {
        throw new NoSuchElementException
      }
      (readAll(new StringText(source.toString.substring(secondLast))), secondLast)
    }

    /**
     * map the specified Iterator of Char to Iterator of StringText with the
     * ICU BreakIterator.
     */
    def mapIterator(source: Iterator[Char]): Iterator[StringText] = {
      val aheadText = source.lookAhead
      val queue = new Queue[StringText]
      scala.collection.Iterator.continually[Option[StringText]](
        if (queue.isEmpty && aheadText.hasNext) {
          val ahead = aheadText.lookAhead(2048)
          queue.enqueue((
            if (ahead.length < 2048) {
              readAll((new StringTextBuilder ++= aheadText.forceTake(2048)).result)
            } else {
              val (ret, read) = readToSecondLast((new StringTextBuilder ++= ahead).result)
              aheadText.forceDrop(read)
              ret
            }): _*)
          Some(queue.dequeue)
        } else if (queue.isEmpty) {
          None
        } else {
          Some(queue.dequeue)
        }).takeWhile(_.isDefined).map(_.get)
    }
  }
}