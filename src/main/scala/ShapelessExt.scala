
import shapeless._
import ops.hlist.Prepend
import ops.nat.{Sum, Pred, ToInt}
import syntax.sized._
import scala.collection.generic.{ CanBuildFrom, IsTraversableLike }
import scala.collection.{ GenTraversable, GenTraversableLike }

object ShapelessExt {

  trait ~~>[F[_, _], R] extends Poly1 {
    def apply[A, B](f : F[A, B]) : R
    implicit def caseUniv[A, B]: Case.Aux[F[A, B], R] = at[F[A, B]](apply(_))
  }


  // HSemiGroup
  trait HSemiGroup[A, B] {
    type Real
    type Out

    def append(a: A, b: B): Out
  }

  // HZero
  trait HZero {
    type Real
    type Zero

    def zero: Zero
  }


  // HMonoid
  trait HMonoid[A, B] extends HZero with HSemiGroup[A, B]

  object HMonoid {
    implicit def HMonoid[A, B, R](
      implicit hs: HSemiGroup[A, B] { type Real = R },
               hz: HZero { type Real = R }
    ) = new HMonoid[A, B] {
      type Real = R
      type Zero = hz.Zero
      type Out = hs.Out

      def zero = hz.zero
      def append(a: A, b: B) = hs.append(a, b)
    }
  }

  // Numeric HMonoid
  implicit def NumericHZero[R : Numeric] = new HZero {
    type Real = R
    type Zero = R

    def zero = implicitly[Numeric[R]].zero
  }

  implicit def NumericHSemiGroup[A <: R, B <: R, R : Numeric] =
    new HSemiGroup[A, B] {
      type Real = R
      type Out = R

      def append(a: A, b: B): Out = implicitly[Numeric[R]].plus(a, b)
    }

  // HLIST HMONOID
  implicit object HListHZero extends HZero {
    type Real = HList
    type Zero = HNil

    def zero = HNil
  }

  implicit def HListHSemiGroup[A <: HList, B <: HList](implicit prepend: Prepend[A, B]) =
    new HSemiGroup[A, B] {
      type Real = HList
      type Out = prepend.Out

      def append(a: A, b: B): Out = prepend(a, b)
    }

  // NAT HMONOID
  implicit object NatHZero extends HZero {
    type Real = Nat
    type Zero = Nat._0

    def zero = Nat._0
  }

  implicit def NatHSemiGroup[A <: Nat, B <: Nat, C <: Nat](implicit sum: Sum.Aux[A, B, C], pred: Pred[C]) =
    new HSemiGroup[A, B] {
      type Real = Nat
      type Out = Succ[pred.Out]

      def append(a: A, b: B): Out = new Succ[pred.Out]
    }

  // Option HMONOID
  implicit def OptionHZero[A] = new HZero {
    type Real = Option[A]
    type Zero = None.type

    def zero = None
  }

  implicit def OptionHSemiGroup[A, B](implicit hm: HMonoid[A, B]) =
    new HSemiGroup[Option[A], Option[B]] {
      type Real = Option[hm.Out]
      type Out = Option[hm.Out]

      def append(a: Option[A], b: Option[B]): Out = for {
        a <- a
        b <- b
      } yield (hm.append(a, b))
    }

  implicit def OptionNoneHSemiGroup[A] =
    new HSemiGroup[Option[A], None.type] {
      type Real = Option[A]
      type Out = Option[A]

      def append(a: Option[A], b: None.type): Out = a
    }

  implicit def NoneOptionHSemiGroup[A] =
    new HSemiGroup[None.type, Option[A]] {
      type Real = Option[A]
      type Out = Option[A]

      def append(a: None.type, b: Option[A]): Out = b
    }

  implicit def SomeHSemiGroup[A, B](implicit hm: HMonoid[A, B]) =
    new HSemiGroup[Some[A], Some[B]] {
      type Real = Option[hm.Out]
      type Out = Option[hm.Out]

      def append(a: Some[A], b: Some[B]): Out = for {
        a <- a
        b <- b
      } yield (hm.append(a, b))
    }

  implicit def SomeNoneHSemiGroup[A] =
    new HSemiGroup[Some[A], None.type] {
      type Real = Option[A]
      type Out = Option[A]

      def append(a: Some[A], b: None.type): Out = a
    }

  implicit def NoneSomeHSemiGroup[A] =
    new HSemiGroup[None.type, Some[A]] {
      type Real = Option[A]
      type Out = Option[A]

      def append(a: None.type, b: Some[A]): Out = b
    }


  // TRAVERSABLES HMONOID
  implicit def ListHZero[A] = new HZero {
    type Real = List[A]
    type Zero = Nil.type

    def zero = Nil
  }

  implicit def MapHZero[A, B] = new HZero {
    type Real = Map[A, B]
    type Zero = Map[A, Nothing]

    def zero = Map.empty[A, Nothing]
  }

  implicit def TraversableHSemiGroup[A, B, E, E0 >: E](
    implicit convA : A => GenTraversableLike[E, A],
             convB : B => GenTraversableLike[E0, B],
             cbf: CanBuildFrom[A, E0, B]
  ) = new HSemiGroup[A, B] {
      type Real = B
      type Out = B

      def append(a: A, b: B): Out = a ++ b
    }

  implicit def TraversableZeroHSemiGroup[A, B, E](
    implicit convA : A => GenTraversableLike[E, A],
             hz: HZero { type Zero = B }
  ) = new HSemiGroup[A, B] {
      type Real = A
      type Out = A

      def append(a: A, b: B): Out = a
    }

  implicit def ZeroTraversableHSemiGroup[A, B, E](
    implicit convB : B => GenTraversableLike[E, B],
             hz: HZero { type Zero = A }
  ) = new HSemiGroup[A, B] {
      type Real = B
      type Out = B

      def append(a: A, b: B): Out = b
    }

  // Sized
  implicit def SizedHZero[R](
    implicit hz: HZero { type Real = R }
  ) = new HZero {
    type Real = Sized[R, Nat]
    type Zero = Sized[hz.Zero, Nat._0]

    def zero = new Sized[hz.Zero, Nat._0](hz.zero)
  }

  implicit def SizedHSemiGroup[A, B, E, E0 >: E, AN <: Nat, BN <: Nat, CN <: Nat](
    implicit convA : A => GenTraversableLike[E, A],
             convB : B => GenTraversableLike[E0, B],
             cbf: CanBuildFrom[A, E0, B],
             hz:  HSemiGroup[A, B] { type Real = B },
             sum: Sum.Aux[AN, BN, CN]
  ) = new HSemiGroup[Sized[A, AN], Sized[B, BN]] {

    type Real = Sized[B, Nat]
    type Out = Sized[B, CN]

    def append(a: Sized[A, AN], b: Sized[B, BN]): Out =
      Sized.wrap[B, CN](a.unsized ++ b.unsized)
  }

  implicit def SizedZeroHSemiGroup[A, B, E, E0 >: E, AN <: Nat, BN <: Nat, CN <: Nat](
    implicit convA : A => GenTraversableLike[E, A],
             hz: HZero { type Real = Sized[A, Nat]; type Zero = Sized[B, BN] }
  ) = new HSemiGroup[Sized[A, AN], Sized[B, BN]] {

    type Real = Sized[A, Nat]
    type Out = Sized[A, AN]

    def append(a: Sized[A, AN], b: Sized[B, BN]): Out = a
  }

}