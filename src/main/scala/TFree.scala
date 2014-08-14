import scalaz._
import Scalaz._

import shapeless._
import poly._

object TFree {


  type FC[F[_], A, B] = A => Free[F, B]
  type FMExp[F[_], A, B] = TFingerTree[({ type l[X, Y] = FC[F, X, Y] })#l, A, B]

  sealed trait Free[S[_], A] {
    type X
    type FH <: FreeView[S, X]
    type FT <: FMExp[S, X, A]

    val head: FH
    val tail: FT
  }

  object Free {
    import TFingerTree._

    type Aux[S[_], A, X0, FH0 <: FreeView[S, X0], FT0 <: FMExp[S, X0, A]] = Free[S, A] { type X = X0 ; type FH = FH0 ; type FT = FT0}

    def apply[S[_], A, X0, FH0 <: FreeView[S, X0], FT0 <: FMExp[S, X0, A]](h: FH0, t: FT0) = new Free[S, A] {
      type X  = X0
      type FH = FH0
      type FT = FT0

      val head = h
      val tail = t
    }

    //case class FM2[S[_], X, A, FH <: FreeView[S, X], FT <: FMExp[S, X, A]](head: FH, tail: FT) extends Free[S, A]

    def fromView[S[_], A, FH <: FreeView[S, A]](h: FH): Free[S, A] =
      Free[S, A, A, FH, Empty[({ type l[X, Y] = FC[S, X, Y] })#l, A]](h, empty[({ type l[X, Y] = FC[S, X, Y] })#l, A])

    def bind[
      S[_], A, B, X0,
      FH  <: Free.Aux[S, A, X0, FH0, FT0],
      FH0 <: FreeView[S, X0],
      FT0 <: FMExp[S, X0, A],
      FT  <: FMExp[S,  A, B],
      FO  <: FMExp[S, X0, B],
      F   <: Poly
    ](f: FH, tc: FT)(implicit
      c: AppendPoly.Case.Aux[FT0, FT, FO]
    ): Free[S, B] = {
      Free[S, B, X0, FH0, FO](f.head, c(f.tail, tc))
    }

    import HMonadImplicits._

    implicit def FreeHMonad[
      S[_], A, B, X0, X1,
      FA   <: Free.Aux[S, A, X0, FH0, FT0],
      FH0  <: FreeView[S, X0],
      FT0  <: FMExp[S, X0, A],
      F    <: Poly, 
      FB   <: Free[S, B],
      FOut0<: FC[S, A, B],
      FOut <: FC[S, X0, B],  //Free.Aux[S, A, X0, FH0, FT1],
      FT1  <: FMExp[S, X0, B]
    ](implicit 
      c  : Case1.Aux[F, A, FB],
      app: AppendPoly.Case.Aux[FT0, FOut0, FOut]
    ) = new HMonad[FA, F] {
      type Out = Free[S, B]
      def bind(l: FA)(f: F) = {
        val ff: FC[S, A, B] = (a: A) => c(a)
        val fc: FMExp[S, A, B] = single[({ type l[X, Y] = FC[S, X, Y] })#l, A, B](ff)
        Free(l.head, app(l.tail, fc) )
      }
    }
  }

}


sealed trait TViewl[S[_[_, _], _, _], C[_, _], +X, Y]

object TViewl {
  case class TEmptyL[S[_[_, _], _, _], C[_, _], X]() extends TViewl[S, C, X, X]

  case class TLeafL[S[_[_, _], _, _], C[_, _], X, Y, Z](head: C[X, Y], tail: S[C, Y, Z]) extends TViewl[S, C, X, Z]
}


sealed abstract class FreeView[S[_], A]

object FreeView {
  case class Pure[S[_], A](a: A) extends FreeView[S, A]
  case class Impure[S[_], A](a: S[TFree.Free[S, A]]) extends FreeView[S, A]
}


case class Get[I, A](f: I => A)

object TFreeTest {
  import TFree._
  import Free._
  import FreeView._

  type It[I, A] = Free[({ type l[T] = Get[I, T]})#l, A]

  def get[I]: It[I, I] = {
    type L[T] = ({ type l[T] = Get[I, T]})#l[T]

    fromView[
      L, I, Impure[L, I]
    ](
      Impure[L, I](Get( (i:I) => 
        fromView[L, I, Pure[L, I]](Pure[L, I](i))
      ))
    )
  }

  // def addGet(i: Int): It[Int, Int] = {
  //   get[Int]
  // }
}


trait TSequence[S[_[_, _], _, _]] {
  def tempty[C[_, _], X]: S[C, X, X]
  def tsingleton[C[_, _], X, Y](c: C[X, Y]): S[C, X, Y]
  //def tappend[C[_, _], X, Y, Z](a: S[C, X, Y], b: S[C, Y, Z]): S[C, X, Z]
  //def tviewl[C[_, _], X, Y](s: S[C, X, Y]): TViewl[S, C, X, Y]
}



// object FreeView {
//   case class Pure[S[_], A](a: A) extends FreeView[S, A]
//   case class Impure[S[_], A](a: S[TFree.Free[S, A]]) extends FreeView[S, A]
// }



// trait TSequence[S[_]] {
//   def tempty[C[_, _], X]: S[C[X, X]]
//   def tsingleton[C[_, _], X, Y](c: C[X, Y]): S[C[X, Y]]
//   def tappend[C[_, _], X, Y, Z](a: S[C[X, Y]], b: S[C[Y, Z]]): S[C[X, Z]]
//   def tviewl[C[_, _], X, Y](s: S[C[X, Y]]): TViewl[S, C, X, Y]
// }


// sealed trait TViewl[S[_], C[_, _], +X, Y]

// object TViewl {
//   case class TEmptyL[S[_], C[_, _], X]() extends TViewl[S, C, X, X]

//   case class TLeafL[S[_], C[_, _], X, Y, Z](head: C[X, Y], tail: S[C[Y, Z]]) extends TViewl[S, C, X, Z]
// }


  //type TCQueue[S] = IndSeq[S]

  // implicit def tcQueueSeq(implicit M: MonadPlus[IndSeq]) = new TSequence[TCQueue] {
  //   def tempty[C[_, _], X]: TCQueue[C[X, X]] = M.empty[C[X, X]]

  //   def tsingleton[C[_, _], X, Y](c: C[X, Y]): TCQueue[C[X, Y]] = M.point(c)

  //   def tappend[C[_, _], X, Y, Z](a: TCQueue[C[X, Y]], b: TCQueue[C[Y, Z]]): TCQueue[C[X, Z]] = {
  //     a.fold(
  //       empty  = (v:Int)            => (b:TCQueue[C[X, Z]]),
  //       single = (v:Int, x:C[X, Y]) => (x:C[X, Z])
  //     )
  //     //M.plus( (a : IndSeq[C[X, Y]]), (b : IndSeq[C[Y, Z]]) )
  //   }

  //   def tviewl[C[_, _], X, Y](s: TCQueue[C[X, Y]]): TViewl[TCQueue, C, X, Y] =
  //     if(s.isEmpty) TViewl.TEmptyL[TCQueue, C, X]
  //     else TViewl.TLeafL(s.head, s.tail)
  // }

  //type FMExp[F[_], A, B] = TCQueue[FC[F, A, B]]

  /*sealed trait Free[S[_], A] {
    import Free._
    import FreeView._
    import TViewl._
    import TFingerTree._

    def toView[F1 <: Poly, F2 <: Poly](
      implicit F: Functor[S],
               TC: TSequence[TFingerTree],
               viewl: TViewlPoly.Case.Aux[FMExp[S, Any, A], TViewl[TFingerTree, ({ type l[X, Y] = FC[S, X, Y] })#l, Any, A]],
               c: Case2.Aux[F1, FMExp[S, Any, Any], FMExp[S, Any, A], FMExp[S, Any, A]]
               //c2: Case1.Aux[F2, FMExp[S, Any, A], TViewl[TFingerTree, ({ type l[X, Y] = FC[S, X, Y] })#l, Any, A]]
    ): FreeView[S, A] = {
      type C[X, Y] = ({ type l[X, Y] = FC[S, X, Y] })#l[X, Y]

      val FM(head, tail) = this

      head match {
        case Pure(x) =>
          //TC.tviewl[C, Any, A](tail) match {
          val a:  TViewl[TFingerTree, ({ type l[X, Y] = FC[S, X, Y] })#l, Any, A] = viewl(tail)
          a match {
            case a: TEmptyL[TFingerTree, C, A] =>
              Pure[S, A](x.asInstanceOf[A])

            case l: TLeafL[TFingerTree, C, Any, Any, A] =>
              bind(l.head(x), l.tail).toView
          }

        case Impure(f) =>
          Impure( F.map(f)(bind((_:Free[S, Any]), tail)) )
      }
    }
  }

  object Free {

    case class FM[S[_], X, A](head: FreeView[S, X], tail: FMExp[S, X, A]) extends Free[S, A]

    def bind[S[_], A, B, F <: Poly](f: Free[S, A], tc: FMExp[S, A, B])(
      implicit  TC: TSequence[TFingerTree],
                c: Case2.Aux[F, FMExp[S, Any, A], FMExp[S, A, B], FMExp[S, Any, B]]
    ): Free[S, B] = {
      val FM(head, tail) = f
      FM(head, c(tail, tc)) //TC.tappend[({ type l[X, Y] = FC[S, X, Y] })#l, Any, A, B](tail, tc))
    }

    def fromView[S[_], A](v: FreeView[S, A])(
      implicit TC: TSequence[TFingerTree]
    ): Free[S, A] = FM(v, TC.tempty[({ type l[X, Y] = FC[S, X, Y] })#l, A])

    // implicit def Monad[S[_]](
    //   implicit TC: TSequence[TFingerTree]
    // ) = new Monad[({ type l[A] = Free[S, A] })#l]  {
    //   import TFingerTree._

    //   def bind[A, B](fm: Free[S, A])(f: A => Free[S, B]) = {
    //     val FM(head, tail) = fm
    //     val s: FMExp[S, A, B] = TC.tsingleton[({ type l[X, Y] = FC[S, X, Y] })#l, A, B](f)
    //     val t: FMExp[S, Any, A] = tail
    //     val a = AppendPoly(t, s)
    //     FM(
    //       head, 
    //       a //TC.tappend[({ type l[X, Y] = FC[S, X, Y] })#l, Any, A, B](tail, )
    //     )
    //   }

    //   def point[A](a: => A) = fromView(FreeView.Pure(a))
    // }

  }*/