package sumito3478

package object collection {
  implicit class RIterator[+A](val self: Iterator[A]) extends RichIterator[A]
}