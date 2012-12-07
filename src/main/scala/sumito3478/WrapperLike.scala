package sumito3478

trait WrapperLike[+Intern] {
  protected[this] val intern: Intern
}