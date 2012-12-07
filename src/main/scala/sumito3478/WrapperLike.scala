package sumito3478

trait WrapperLike[+Repr] {
  protected[this] val self: Repr
}