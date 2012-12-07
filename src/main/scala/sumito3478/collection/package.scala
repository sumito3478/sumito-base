package sumito3478

package object collection {
  implicit class ImplicitRichIterator[+A](
    val self: Iterator[A]) extends RichIterator[A]
}