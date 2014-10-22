package `lazy`


import annotation.tailrec
import core._

sealed abstract class TFreeView[S[_], A]

object TFreeView {
  case class Pure[S[_], A](a: A) extends TFreeView[S, A]
  case class Impure[S[_], A](a: S[TFree[S, A]]) extends TFreeView[S, A]
}

sealed trait TFree[S[_], A] {
  import TFree._
  import TFreeView._
  import scalaz.{Functor, Monad, ~>}
  import scalaz.std.function._
  import TFingerTree.TFingerTreeSeq

  def toView(implicit F: Functor[S]): TFreeView[S, A] = TFree.toView(this)

  def map[B](f: A => B): TFree[S, B] = {
    this.flatMap( (a:A) =>  fromView(Pure(f(a))) )
  }

  def flatMap[B](f: A => TFree[S, B]): TFree[S, B] = {
    type FCS[A, B] = ({ type l[X, Y] = FC[S, X, Y] })#l[A, B]
    this match {
      case free: FM[S, x, A] =>
        FM(
          free.head,
          TFingerTreeSeq.tappend[FCS, x, A, B](
            free.tail,
            TFingerTreeSeq.tsingleton[FCS, A, B](f)
          )
        )
    }
  }

  final def mapSuspension[T[_]](f: S ~> T)(implicit S: Functor[S], T: Functor[T]): TFree[T, A] = {
    TFree.fromView(
      toView match {
        case Pure(a) => Pure[T, A](a)
        case Impure(a) => Impure[T, A](f(S.map(a){ tf => tf mapSuspension f }))
      }
    )
  }

  final def foldMap[M[_]](f: S ~> M)(implicit S: Functor[S], M: Monad[M]): M[A] = {
    toView match {
      case Pure(a)   => M.point(a)
      case Impure(a) => M.bind(f(a)){ _.foldMap(f) }
    }
  }

  final def foldMapT(f: S ~> Trampoline)(implicit S: Functor[S]): Trampoline[A] = {
    @tailrec def step(free: TFree[S, A]): Trampoline[A] = {
      free.toView match {
        case Pure(a)   => Trampoline.done(a)
        case Impure(a) => step(f(a).run)
      }
    }

    step(this)
  }

  /** Runs a trampoline all the way to the end, tail-recursively. */
  def run(implicit ev: TFree[S, A] =:= Trampoline[A]): A = {
    ev(this).go(_())
  }

  /** Runs to completion, using a function that extracts the resumption from its suspension functor. */
  final def go(f: S[TFree[S, A]] => TFree[S, A])(implicit S: Functor[S]): A = {
    @tailrec def go2(t: TFree[S, A]): A = {
      t.toView match {
        case Impure(a) => go2(f(a))
        case Pure(a)   => a
      }
    }
    go2(this)
  }

}

object TFree extends TCopoyo {
  import TFreeView._
  import scalaz.{Monad, Functor, Coyoneda}
  import TFingerTree.TFingerTreeSeq

  type FC[F[_], A, B] = A => TFree[F, B]
  type FMExp[F[_], A, B] = TFingerTree[({ type l[X, Y] = FC[F, X, Y] })#l, A, B]

  case class FM[S[_], X, A](
    head: TFreeView[S, X],
    tail: FMExp[S, X, A]
  ) extends TFree[S, A]

  def fromView[S[_], A](h: TFreeView[S, A]): TFree[S, A] =
    FM(h, TFingerTree.empty[({ type l[X, Y] = FC[S, X, Y] })#l, A])

  @tailrec def toView[S[_], A](free: TFree[S, A])(
    implicit F: Functor[S]
  ): TFreeView[S, A] = {
    type FCS[A, B] = ({ type l[X, Y] = FC[S, X, Y] })#l[A, B]

    free match {
      case f:FM[S, x, A] => f.head match {
        case Pure(x) =>
          TFingerTreeSeq.tviewl[FCS, x, A](f.tail) match {
            case _: TViewl.EmptyL[TFingerTree, FCS, x] =>
              Pure(x)

            case l: TViewl.LeafL[TFingerTree, FCS, u, v, A] =>
              toView(
                l.head(x.asInstanceOf[u]) match {
                  case f2: FM[S, x, v] =>
                    FM(
                      f2.head,
                      TFingerTreeSeq.tappend[FCS, x, v, A](f2.tail, l.tail())
                    )
                }
              )
          }
        case Impure(a) =>
          Impure(F.map(a){
            case f2: FM[S, y, x] =>
              FM(f2.head, TFingerTreeSeq.tappend[FCS, y, x, A](f2.tail, f.tail))
          })
      }
    }
  }

  implicit def TFreeMonad[S[_]] = new Monad[({ type l[A] = TFree[S, A] })#l] {

    def point[A](a: => A): TFree[S, A] = fromView(Pure(a))

    def bind[A, B](fa: TFree[S, A])(f: A => TFree[S, B]): TFree[S, B] = fa flatMap f

  }

  type TFreeC[S[_], A] = TFree[({type f[x] = Coyoneda[S, x]})#f, A]

  type Trampoline[A] = TFree[Function0, A]

  object Trampoline {

    def done[A](a: A): Trampoline[A] =
      TFree.fromView(TFreeView.Pure[Function0, A](a))

    def delay[A](a: => A): Trampoline[A] =
       suspend(done(a))

    def suspend[A](a: => Trampoline[A]): Trampoline[A] =
      TFree.fromView(TFreeView.Impure[Function0, A](() => a))

  }

  type Source[A, B] = TFree[({type f[x] = (A, x)})#f, B]

}


trait TCopoyo {
  import scalaz.{Coyoneda, Functor, Unapply}
  import shapeless.ops.coproduct.{Inject, Selector}
  import shapeless.{Coproduct, Inl, Inr, CNil, :+:, Poly1, Id}
  import shapeless.poly._

  /** Suspends a value within a functor in a single step. */
  def liftF[S[_], A](value: => S[A])(implicit S: Functor[S]): TFree[S, A] =
    TFree.fromView[S, A](TFreeView.Impure[S, A](S.map(value){ v =>
      TFree.fromView[S, A](TFreeView.Pure[S, A](v))
    }))

  /** A version of `liftF` that infers the nested type constructor. */
  def liftFU[MA](value: => MA)(implicit MA: Unapply[Functor, MA]): TFree[MA.M, MA.A] =
    liftF(MA(value))(MA.TC)

  /** A free monad over a free functor of `S`. */
  def liftFC[S[_], A](s: S[A]): TFree.TFreeC[S, A] =
    liftFU(Coyoneda lift s)

  class TCopoyo[C[_] <: Coproduct] {
    def apply[F[_], A](fa: F[A])(implicit inj: Inject[C[A], F[A]]): TFree.TFreeC[C, A] =
      liftFC(Coproduct[C[A]](fa))
  }

  object TCopoyo {
    def apply[C[_] <: Coproduct] = new TCopoyo[C]
  }

  implicit def TInject[F[_], A, C[_] <: Coproduct](f: F[A])(
    implicit inj: Inject[C[A], F[A]]
  ): TFree.TFreeC[C, A] = liftFC(Coproduct[C[A]](f))
}












