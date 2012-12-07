package sumito3478

trait WrapperLike[+Repr] {
  protected[this] val intern: Repr
}