package info.sumito3478

trait WrapperLike[+Intern] {
  protected[this] val intern: Intern
}