/*
 * Copyright 2014 Pascal Voitot (@mandubian)
 *
 */
import scalaz.{Free, Coyoneda, Functor, Unapply}
import Coyoneda.CoyonedaF

import shapeless.ops.coproduct.{Inject, Selector}
import shapeless.{Coproduct, Inl, Inr, CNil, :+:, Poly1, Id}
import shapeless.poly._

object Shapoyo {

  implicit class RichNatT[F[_], R[_]](val f: F ~> R) extends AnyVal {

    def ||:[G[_]](g: G ~> R) = {

      new ~>[({ type l[T] = G[T] :+: F[T] :+: CNil })#l, R] {
        def apply[T](c: G[T] :+: F[T] :+: CNil) = c match {
          case Inl(h) => g(h)
          case Inr(Inl(t)) => f(t)
          case _ => throw new RuntimeException("impossible case")
        }
      }
    }
  }

  implicit class RichNatT2[G[_], H[_], R[_]](val g: ({ type l[T] = (G[T] :+: H[T] :+: CNil) })#l ~> R) {

    def ||:[F[_]](f: F ~> R) = {

      new ~>[({ type l[T] = F[T] :+: G[T] :+: H[T] :+: CNil })#l, R] {
        def apply[T](c: F[T] :+: G[T] :+: H[T] :+: CNil) = c match {
          case Inl(h) => f(h)
          case Inr(t) => g(t)
          case _ => throw new RuntimeException("impossible case")
        }
      }
    }
  }

  implicit class RichNatT3[G[_], H[_], I[_], R[_]](val g: ({ type l[T] = (G[T] :+: H[T] :+: I[T] :+: CNil) })#l ~> R) {

    def ||:[F[_]](f: F ~> R) = {
      //type L[T] = F[T] :+: G[T]

      new ~>[({ type l[T] = F[T] :+: G[T] :+: H[T] :+: I[T] :+: CNil })#l, R] {
        def apply[T](c: F[T] :+: G[T] :+: H[T] :+: I[T] :+: CNil) = c match {
          case Inl(h) => f(h)
          case Inr(t) => g(t)
          case _ => throw new RuntimeException("impossible case")
        }
      }
    }
  }

  def liftCoyo[F[_], G[_]](fg: F ~> G): CoyonedaF[F]#A ~> CoyonedaF[G]#A =
    new (Coyoneda.CoyonedaF[F]#A ~> Coyoneda.CoyonedaF[G]#A) {
      def apply[A](c: Coyoneda[F, A]) = {
        Coyoneda.apply(fg(c.fi))(c.k)
      }
    }

  // def iso[F[_]: Functor]: CoyonedaF[F]#A <~> F =
  //   new IsoFunctorTemplate[CoyonedaF[F]#A, F] {
  //     def from[A](fa: F[A]) = lift(fa)
  //     def to[A](fa: Coyoneda[F, A]) = fa.run
  //   }

  def liftCoyoLeft[F[_], G[_]: Functor](fg: F ~> G): CoyonedaF[F]#A ~> G = {
    type CF[A] = Coyoneda[F, A]
    type CG[A] = Coyoneda[G, A]

    val m: (CF ~> CG) = liftCoyo(fg)
    // val n: (CG ~> G) = iso[G].to
    // n compose m

    new (CF ~> G) {
      def apply[A](c: CF[A]) = m(c).run
    }
  }

  implicit def toScalazNat[F[_], G[_]](nat: F ~> G) = new scalaz.~>[F, G] {
    def apply[T](ft: F[T]): G[T] = nat(ft)
  }

  def copoyo[C[_] <: Coproduct, F[_], A](fa: F[A])(implicit inj: Inject[C[A], F[A]]): Free.FreeC[C, A] =
    Free.liftFC(Coproduct[C[A]](fa))

  class Copoyo[C[_] <: Coproduct] {
    def apply[F[_], A](fa: F[A])(implicit inj: Inject[C[A], F[A]]): Free.FreeC[C, A] =
      Free.liftFC(Coproduct[C[A]](fa))
  }

  object Copoyo {
    def apply[C[_] <: Coproduct] = new Copoyo[C]
  }

  class Program[C[_] <: Coproduct] {
    type Copro[A]  = C[A]
    type Copoyo[A] = Coyoneda[Copro, A]

    implicit def CopoyoApp[F[_], A](f: F[A])(implicit inj: Inject[C[A], F[A]]) = Copoyo[C](f)

    def exec[Out](interpreters: Copro ~> Id)(implicit instructions: Free.FreeC[Copro, Out]): Free[Id, Out] = {
      val is: Copoyo ~> Id = liftCoyoLeft(interpreters)
      instructions.mapSuspension(is)
    }
  }

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

  // def tcopoyo[C[_] <: Coproduct, F[_], A](fa: F[A])(implicit inj: Inject[C[A], F[A]]): TFree.TFreeC[C, A] = {
  //   type Copro[A]  = C[A]
  //   type Copoyo[A] = Coyoneda[Copro, A]

  //   val c: Copoyo[A] = Coyoneda lift Coproduct[C[A]](fa)

  //   liftFC(Coproduct[C[A]](fa))
  // }

  class TCopoyo[C[_] <: Coproduct] {
    def apply[F[_], A](fa: F[A])(implicit inj: Inject[C[A], F[A]]): TFree.TFreeC[C, A] =
      liftFC(Coproduct[C[A]](fa))
  }

  object TCopoyo {
    def apply[C[_] <: Coproduct] = new TCopoyo[C]
  }


  // object Program {
  //   def apply[C[_] <: Coproduct] = new Program[C]
  // }
  import shapeless.{Poly, HList, Unpack2}

  case class Merge[+F, G](f: F, g: G) extends Poly

  object Merge extends Merge2

  trait Merge1 {
    implicit def mergeCase1A[MG, A <: Poly, B <: Poly, ML <: HList, MR](
      implicit unpack: Unpack2[MG, Merge, A, B],
               c : Case.Aux[A, ML, MR]) = new Case[MG, ML] {
      type Result = MR
      val value = (t : ML) => c(t)
    }

    implicit def mergeCase1B[MG, A <: Poly, B <: Poly, ML <: HList, MR](
      implicit unpack: Unpack2[MG, Merge, A, B],
               c : Case.Aux[B, ML, MR]) = new Case[MG, ML] {
      type Result = MR
      val value = (t : ML) => c(t)
    }
  }


  trait Merge2 extends Merge1 {
    implicit def mergeCase2A[MG, MG2, A <: Poly, B <: Poly, C <: Poly, ML <: HList, MR](
      implicit unpack1: Unpack2[MG, Merge, MG2, C], unpack2: Unpack2[MG2, Merge, A, B],
               c : Case.Aux[A, ML, MR]) = new Case[MG, ML] {
      type Result = MR
      val value = (t : ML) => c(t)
    }

    implicit def mergeCase2B[MG, MG2, A <: Poly, B <: Poly, C <: Poly, ML <: HList, MR](
      implicit unpack1: Unpack2[MG, Merge, MG2, C], unpack2: Unpack2[MG2, Merge, A, B],
               c : Case.Aux[B, ML, MR]) = new Case[MG, ML] {
      type Result = MR
      val value = (t : ML) => c(t)
    }

    implicit def mergeCase2C[MG, MG2, A <: Poly, B <: Poly, C <: Poly, ML <: HList, MR](
      implicit unpack1: Unpack2[MG, Merge, MG2, C], unpack2: Unpack2[MG2, Merge, A, B],
               c : Case.Aux[C, ML, MR]) = new Case[MG, ML] {
      type Result = MR
      val value = (t : ML) => c(t)
    }
  }

  implicit class RichMerge[F <: Poly](val m: F) extends AnyVal {
    def |+|[G <: Poly](other: G) = Merge(m, other)
  }

  // trait UnaryAConstraint[C <: Coproduct, A]

  // type +->+[A] = {
  //   type λ[C <: Coproduct] = UnaryAConstraint[C, A]
  // }

  // implicit def hnilUnaryTC[A] = new UnaryAConstraint[CNil, A] {}
  // implicit def hlistUnaryTC1[F[_], C <: Coproduct, A](implicit utct : UnaryAConstraint[C, A]) =
  //   new UnaryAConstraint[F[A] :+: C, A] {}

}




