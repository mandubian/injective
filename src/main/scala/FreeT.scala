import scalaz._
import Scalaz._

import shapeless._
import poly._

object FreeT {

  type FC[F[_], A, B] = A => Free[F, B]
  type FMExp[F[_], A, B] = FingerTree //TFingerTree[({ type l[X, Y] = FC[F, X, Y] })#l, A, B]

  sealed abstract class FreeView[S[_], A]

  object FreeView {
    case class Pure[S[_], A](a: A) extends FreeView[S, A]
    case class Impure[S[_], A](a: S[FreeT.Free[S, A]]) extends FreeView[S, A]
  }

  sealed trait Free[S[_], A] {
    type X
    type FH <: FreeView[S, X]
    type FT <: FMExp[S, X, A]

    val head: FH
    val tail: FT
  }

  object Free {
    import FreeView._

    def fromView[S[_], A, FH0 <: FreeView[S, A]](v: FH0) = new Free[S, A] {
      type X = A
      type FH = FH0
      type FT = FingerTree.Empty.type

      val head = v
      val tail = FingerTree.empty()
    }

    def point[S[_], A](a: A) = fromView[S, A, Pure[S, A]](Pure[S, A](a))
  }
}