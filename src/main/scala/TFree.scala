import scalaz._
import Scalaz._

import shapeless._
import poly._

trait TSequence[S[_[_, _], _, _]] {
  def tempty[C[_, _], X]: S[C, X, X]
  def tsingleton[C[_, _], X, Y](c: C[X, Y]): S[C, X, Y]
  def tappend[C[_, _], X, Y, Z](a: S[C, X, Y], b: S[C, Y, Z]): S[C, X, Z]
  def tviewl[C[_, _], X, Y](s: S[C, X, Y]): TViewl[S, C, X, Y]
}

sealed trait TViewl[S[_[_, _], _, _], C[_, _], +X, Y]

object TViewl {
  case class EmptyL[S[_[_, _], _, _], C[_, _], X]() extends TViewl[S, C, X, X]

  case class LeafL[S[_[_, _], _, _], C[_, _], X, Y, Z](head: C[X, Y], tail: S[C, Y, Z]) extends TViewl[S, C, X, Z]
}


sealed abstract class TFreeView[S[_], A]

object TFreeView {
  case class Pure[S[_], A](a: A) extends TFreeView[S, A]
  case class Impure[S[_], A](a: S[TFree[S, A]]) extends TFreeView[S, A]
}

sealed trait TFree[S[_], A]

object TFree {
  import TFreeView._

  type FC[F[_], A, B] = A => TFree[F, B]
  type FMExp[F[_], A, B] = TFingerTree[({ type l[X, Y] = FC[F, X, Y] })#l, A, B]

  case class FM[S[_], X, A](head: TFreeView[S, X], tail: FMExp[S, X, A]) extends TFree[S, A]

  def fromView[S[_], A](h: TFreeView[S, A]): TFree[S, A] =
    FM(h, TFingerTree.empty[({ type l[X, Y] = FC[S, X, Y] })#l, A])

  def toView[S[_], A](free: TFree[S, A])(
    implicit F: Functor[S], TS: TSequence[TFingerTree]
  ): TFreeView[S, A] = free match {
    case f:FM[S, x, A] => f.head match {
      case Pure(x) => TS.tviewl[({ type l[X, Y] = FC[S, X, Y] })#l, x, A](f.tail) match {
        case _: TViewl.EmptyL[TFingerTree, ({ type l[X, Y] = FC[S, X, Y] })#l, x] => Pure(x)
        case l: TViewl.LeafL[TFingerTree, ({ type l[X, Y] = FC[S, X, Y] })#l, u, v, A] =>
          l.head(x.asInstanceOf[u]) match {
            case f: FM[S, x, v] => toView(FM(f.head, TS.tappend[({ type l[X, Y] = FC[S, X, Y] })#l, x, v, A](f.tail, l.tail)))
          }
      }
      case Impure(a) => Impure(F.map(a){
        case f2: FM[S, y, x] =>
          FM(f2.head, TS.tappend[({ type l[X, Y] = FC[S, X, Y] })#l, y, x, A](f2.tail, f.tail))
      })
    }
  }

  implicit def TFreeMonad[S[_]](
    implicit TS: TSequence[TFingerTree]
  ) = new Monad[({ type l[A] = TFree[S, A] })#l] {

    def point[A](a: => A): TFree[S, A] = fromView(Pure(a))

    def bind[A, B](fa: TFree[S, A])(f: A => TFree[S, B]): TFree[S, B] = fa match {
      case free: FM[S, x, A] =>
        FM(
          free.head,
          TS.tappend[({ type l[X, Y] = FC[S, X, Y] })#l, x, A, B](
            free.tail,
            TS.tsingleton[({ type l[X, Y] = FC[S, X, Y] })#l, A, B](f)
          )
        )
    }
  }
}





// case class Get[I, A](f: I => A)

// object TTFreeTest {
//   import TTFree._
//   import TFree._
//   import TFreeView._

//   type It[I, A] = TFree[({ type l[T] = Get[I, T]})#l, A]

//   def get[I]: It[I, I] = {
//     type L[T] = ({ type l[T] = Get[I, T]})#l[T]

//     fromView[
//       L, I, Impure[L, I]
//     ](
//       Impure[L, I](Get( (i:I) => 
//         fromView[L, I, Pure[L, I]](Pure[L, I](i))
//       ))
//     )
//   }

//   // def addGet(i: Int): It[Int, Int] = {
//   //   get[Int]
//   // }
// }




