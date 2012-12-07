package sumito3478.collection

object Riches {
  implicit class RIterator[+A](val self: Iterator[A]) extends RichIterator[A]
}