package sumito3478

package object collection {
  implicit class ImplicitRichIterator[+A](
    val intern: Iterator[A]) extends RichIterator[A]
}