/*
 * Copyright 2014 Pascal Voitot (@mandubian)
 *
 */
import shapeless.ops.coproduct.{Inject, Selector}
import shapeless.{Coproduct, Inl, Inr, CNil, :+:, Poly1, Id, DepFn1}
//import shapeless.poly._

import scalaz.{Free, Coyoneda, Functor, Unapply, ~>, Monad}
import Coyoneda.CoyonedaF


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

  implicit class RichNatT4[G[_], H[_], I[_], J[_], R[_]](val g: ({ type l[T] = (G[T] :+: H[T] :+: I[T] :+: J[T] :+: CNil) })#l ~> R) {

    def ||:[F[_]](f: F ~> R) = {
      //type L[T] = F[T] :+: G[T]

      new ~>[({ type l[T] = F[T] :+: G[T] :+: H[T] :+: I[T] :+: J[T] :+: CNil })#l, R] {
        def apply[T](c: F[T] :+: G[T] :+: H[T] :+: I[T] :+: J[T] :+: CNil) = c match {
          case Inl(h) => f(h)
          case Inr(t) => g(t)
          case _ => throw new RuntimeException("impossible case")
        }
      }
    }
  }

  implicit def CoproductFunctor1[F[_]](implicit F: Functor[F]) = 
    new Functor[({ type l[A] = F[A] :+: CNil })#l] {

      def map[A, B](fa: F[A] :+: CNil)(f: A => B): F[B] :+: CNil = fa match {
        case Inl(h) => Coproduct[F[B] :+: CNil](F.map(h)(f))
        case Inr(t) => throw new RuntimeException("impossible case")
        case _ => throw new RuntimeException("impossible case")
      }

    }

  implicit def CoproductFunctor2[F[_], G[_]](implicit F: Functor[F], G: Functor[({ type l[A] = G[A] :+: CNil })#l]) = 
    new Functor[({ type l[A] = F[A] :+: G[A] :+: CNil })#l] {
      import Coproduct._

      def map[A, B](fa: F[A] :+: G[A] :+: CNil)(f: A => B): F[B] :+: G[B] :+: CNil = fa match {
        case Inl(h) => Coproduct[F[B] :+: G[B] :+: CNil](F.map(h)(f))
        case Inr(t) => G.map(t)(f).extendLeft[F[B]]
        case _ => throw new RuntimeException("impossible case")
      }

    }

  implicit def CoproductFunctor3[F[_], G[_], H[_]](implicit FH: Functor[F], FT: Functor[({ type l[A] = G[A] :+: H[A] :+: CNil })#l]) = 
    new Functor[({ type l[A] = F[A] :+: G[A] :+: H[A] :+: CNil })#l] {
      import Coproduct._
      
      def map[A, B](fa: F[A] :+: G[A] :+: H[A] :+: CNil)(f: A => B): F[B] :+: G[B] :+: H[B] :+: CNil = fa match {
        case Inl(h) => Coproduct[F[B] :+: G[B] :+: H[B] :+: CNil](FH.map(h)(f))
        case Inr(t) => FT.map(t)(f).extendLeft[F[B]]
        case _ => throw new RuntimeException("impossible case")
      }

    }

  def liftCoyo[F[_], G[_]](fg: F ~> G): CoyonedaF[F]#A ~> CoyonedaF[G]#A =
    new (Coyoneda.CoyonedaF[F]#A ~> Coyoneda.CoyonedaF[G]#A) {
      def apply[A](c: Coyoneda[F, A]) = {
        Coyoneda.apply(fg(c.fi))(c.k)
      }
    }

  def liftCoyoLeft[F[_], G[_]: Functor](fg: F ~> G): CoyonedaF[F]#A ~> G = {
    type CF[A] = Coyoneda[F, A]
    type CG[A] = Coyoneda[G, A]

    val m: (CF ~> CG) = liftCoyo(fg)

    new (CF ~> G) {
      def apply[A](c: CF[A]) = m(c).run
    }
  }

  def liftFree[F[_]: Functor, G[_]: Monad](fg: F ~> G): ({ type l[A] = Free[F, A] })#l ~> G = {
    new (({ type l[A] = Free[F, A] })#l ~> G) {
      def apply[A](free: Free[F, A]) = free.foldMap(fg)
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


  trait SubCoproduct[Sub <: Coproduct, Super <: Coproduct] extends DepFn1[Sub] {
    type Out = Super
  }

  object SubCoproduct {
    implicit def single[H, T <: Coproduct, Super <: Coproduct](
      implicit inj: Inject[Super, H], sub: SubCoproduct[T, Super]
    ) = new SubCoproduct[H :+: T, Super] {

      def apply(c: H :+: T) = c match {
        case Inl(h) => inj(h)
        case Inr(t) => sub(t)
      }
    }
  }

  /*trait CoproductUnapply[C[_], CH[_] <: Coproduct]

  object CoproductUnapply {
    implicit def last[H[_]] = 
      new CoproductUnapply[({ type l[A] = H[A] :+: CNil })#l, ({ type l[A] = H[A] :+: CNil })#l] {}

    implicit def headTail[H[_], T[_] <: Coproduct, TH[_] <: Coproduct](implicit un: CoproductUnapply[T, TH]) = 
      new CoproductUnapply[({ type l[A] = H[A] :+: T })#l, ({ type l[A] = H[A] :+: TH[A] })#l] {}
  }

  trait HInject[C[_] <: Coproduct, I[_]] {
    def apply[A](i: I[A]): C[A]
  }

  object HInject {
    def apply[C[_] <: Coproduct, I[_]](implicit hinject: HInject[C, I]): HInject[C, I] = hinject

    // implicit def tlInject[H[_], T[_] <: Coproduct, I[_]](implicit tlInj : HInject[T, I]): HInject[({ type l[A] = H[A] :+: T[A] })#l, I] = 
    //   new HInject[({ type l[A] = H[A] :+: T[A] })#l, I] {
    //     def apply[A](i: I[A]): H[A] :+: T[A] = Inr(tlInj(i))
    //   }

    // implicit def hdInject[T[_] <: Coproduct, H[_]]: HInject[({ type l[A] = H[A] :+: T[A] })#l, H] = 
    //   new HInject[({ type l[A] = H[A] :+: T[A] })#l, H] {
    //     def apply[A](i: H[A]): H[A] :+: T[A] = Inl(i)
    //   }

    // implicit def hdInject[H[_], T[_] <: Coproduct](implicit un: CoproductUnapply[T]) = 
    //   new HInject[({ type l[A] = H[A] :+: T[A] })#l, H] {
    //     def apply[A](i: H[A]): H[A] :+: T[A] = Inl(i)
    //   }
  }


  trait SubCoproductNat[C[_] <: Coproduct, D[_] <: Coproduct] extends ~>[C, D]

  object SubCoproductNat {
    implicit def single[H[_], T[_] <: Coproduct, T2[_] <: Coproduct](
      implicit inj: HInject[T2, H], sub: SubCoproductNat[T, T2]
    ) = new SubCoproductNat[({ type l[A] = H[A] :+: T[A] })#l, T2] {

      def apply[A](c: H[A] :+: T[A]) = c match {
        case Inl(h) => inj(h)
        case Inr(t) => sub(t)
      }
    }
  }


  def extendCoproduct[C <: Coproduct, D <: Coproduct](c: C)(
    implicit sub: SubCoproduct[C, D]
  ): D = sub(c)

  def extendCoproductNat[
    F[_] <: Coproduct, G[_] <: Coproduct
  ](implicit sub:SubCoproductNat[F, G]) = {
    new ~>[F, G] {
      def apply[A](fa: F[A]) = sub(fa)
    }
  }
*/
  // class Program[C[_] <: Coproduct] {
  //   type Copro[A]  = C[A]
  //   type Copoyo[A] = Coyoneda[Copro, A]

  //   implicit def CopoyoApp[F[_], A](f: F[A])(implicit inj: Inject[C[A], F[A]]) = Copoyo[C](f)

  //   def exec[Out](interpreters: Copro ~> Id)(implicit instructions: Free.FreeC[Copro, Out]): Free[Id, Out] = {
  //     val is: Copoyo ~> Id = liftCoyoLeft(interpreters)
  //     instructions.mapSuspension(is)
  //   }
  // }

  // object Program {
  //   def apply[C[_] <: Coproduct] = new Program[C]
  // }
  //import shapeless.{Poly, HList, Unpack2}

  /*case class Merge[+F, G](f: F, g: G) extends Poly

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
  }*/

}




