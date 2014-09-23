

sealed trait FreeZ[S[_], A] {

  import FreeZ._


}

object FreeZ {
  import scalaz.IndSeq

  type FastQ[R[_, _], A, B] = IndSeq[Any]

  type FC[F[_], A, B] = A => FreeZ[F, B]
  type FMExp[F[_], A, B] = FastQ[({ type l[X, Y] = FC[F, X, Y] })#l, A, B]

  case class FM[S[_], X, A](head: TFreeView[S, X], tail: FMExp[S, X, A]) extends FreeZ[S, A]


  implicit object FastQSeq extends TSequence[FastQ] {
    def tempty[C[_, _], X]: FastQ[C, X, X] = IndSeq[Any]()

    def tsingleton[C[_, _], X, Y](c: => C[X, Y]): FastQ[C, X, Y] = IndSeq[Any](c)

    def tappend[C[_, _], X, Y, Z](a: FastQ[C, X, Y], b: => FastQ[C, Y, Z]): FastQ[C, X, Z] = {
      a ++ b
    }

    def tviewl[C[_, _], X, Y](s: FastQ[C, X, Y]): TViewl[FastQ, C, X, Y] = {
      val v = s.self.viewl
      v.headOption match {
        case None    => TViewl.EmptyL[FastQ, C, X]().asInstanceOf[TViewl[FastQ, C, X, Y]]
        case Some(h) => v.tailOption match {
          case None     => TViewl.LeafL[FastQ, C, X, Y, Y](h.asInstanceOf[C[X, Y]], IndSeq[Any]())
          case Some(t)  => TViewl.LeafL[FastQ, C, X, Any, Y](h.asInstanceOf[C[X, Any]], new IndSeq(t))
        }
      }

      /*s match {
        case _:Empty[C, X] => TViewl.EmptyL[TFingerTree, C, X]()
        case t: Single[C, X, Y] => TViewl.leafL[TFingerTree, C, X, Y, Y](t.a, TFingerTree.empty[C, Y]())
        case t: Deep[C, X, u, v, Y] =>
          Digit.toList(t.prefix) match {
            case hh ::: tt => 
              TViewl.leafL(
                hh, 
                TFingerTree.deepL(tt, t.middle(), t.suffix)
              )
          }
      }*/
    }
  }
}