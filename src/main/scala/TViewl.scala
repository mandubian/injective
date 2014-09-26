package core

sealed trait TViewl[S[_[_, _], _, _], C[_, _], +X, Y]

object TViewl {
  case class EmptyL[S[_[_, _], _, _], C[_, _], X]() extends TViewl[S, C, X, X]

  case class LeafL[S[_[_, _], _, _], C[_, _], X, Y, Z](
    head: C[X, Y],
    tail: () => S[C, Y, Z]
  ) extends TViewl[S, C, X, Z]
}