trait TSequence[S[_[_, _], _, _]] {
  def tempty[C[_, _], X]: S[C, X, X]

  def tsingleton[C[_, _], X, Y](c: => C[X, Y]): S[C, X, Y]

  def tappend[C[_, _], X, Y, Z](a: S[C, X, Y], b: => S[C, Y, Z]): S[C, X, Z]

  def tviewl[C[_, _], X, Y](s: => S[C, X, Y]): TViewl[S, C, X, Y]
}
