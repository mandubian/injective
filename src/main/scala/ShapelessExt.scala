
import shapeless._
import poly.Case1
import ops.hlist.{Prepend, Mapper}
import ops.nat.{Sum, Pred, ToInt}
import syntax.sized._
import scala.collection.generic.{ CanBuildFrom, IsTraversableLike }
import scala.collection.{ GenTraversable, GenTraversableLike }

object ShapelessExt
  extends HFunctors
  with HMonoids
  with HApplicatives {

  trait ~~>[F[_, _], R] extends Poly1 {
    def apply[A, B](f : F[A, B]) : R
    implicit def caseUniv[A, B]: Case.Aux[F[A, B], R] = at[F[A, B]](apply(_))
  }

}

object ShapelessExtImplicits
  extends HFunctorImplicits
  with HMonoidImplicits
  with HApplicativeImplicits


trait HApplicatives extends HApplies with HPoints with HFunctors {

  trait HApplicative[HA, HF] extends HApply[HA, HF] {
    type Up

    val pointer: UPoint[Up]

    def point[A, O <: Up](a: A): pointer.Out[A] = pointer(a)

  }

  trait Rel[HA, HF, F] {
    type Out
  }

  object Rel {
    type Aux[HA, HF, F, HO] = Rel[HA, HF, F] { type Out = HO }

    implicit def HListRel[HA <: HList, HT <: HList, F] = new Rel[HA, F :: HT, F] {
      type Out = F :: HNil
    }

    implicit def ListRel[A, F] = new Rel[List[A], List[F], F] {}
  }
}

object HApplicativeImplicits extends HApplicativeImplicits

trait HApplicativeImplicits
  extends HApplicatives
  with HApplyImplicits
  with HPointImplicits {

  implicit def happlicative[HA, HF, U](
    implicit happ: HApply.Up[HA, HF, U],
             p: UPoint[U]
  ) = new HApplicative[HA, HF] {
    type Out = happ.Out
    type Up = happ.Up

    val pointer = p

    def ap(ha: HA)(fs: => HF) = happ.ap(ha)(fs)
  }

  implicit def hfunctor[HA, HF, F <: Poly, HO](
    implicit  happ: HApplicative[HA, HF],
              rel: Rel.Aux[HA, HF, F, HO],
              hpointed: HPoint.Aux[F, HO],
              happ1: HApplicative[HA, HO]
  ) = new HFunctor[HA, F] {
    type Out = happ1.Out

    def map(ha: HA)(f: F): Out = happ1.ap(ha)(hpointed(f))
  }
}

trait HPoints {
  trait UPoint[T] {
    type Out[A] <: T
    def apply[A](a: A): Out[A]
  }

  trait HPoint[H] extends DepFn1[H] { type Out }

  object HPoint {
    type Aux[H, O] = HPoint[H] { type Out = O }
  }

}

object HPointImplicits extends HPointImplicits

trait HPointImplicits extends HPoints {
  implicit def HListHPoint[A] = new HPoint[A] {
    type Out = A :: HNil
    def apply(a: A) = a :: HNil
  }

  implicit def HListUPoint = new UPoint[HList] {
    type Out[A] = A :: HNil
    def apply[A](a: A) = a :: HNil
  }

  implicit def ListHPoint[A] = new HPoint[A] {
    type Out = List[A]
    def apply(a: A) = List(a)
  }

  implicit def ListUPoint = new UPoint[List[_]] {
    type Out[A] = List[A]
    def apply[A](a: A) = List(a)
  }
}



trait HApplies {

  trait HApplier[HF, In] extends DepFn2[In, HF] { type Out }

  trait HApply[HA, HF]{
    type Up
    type Out

    def ap(ha: HA)(fs: => HF): Out
  }

  object HApply {
    type Aux[In, HF, Out0] = HApply[In, HF] { type Out = Out0 }
    type Up[In, HF, Up0] = HApply[In, HF] { type Up = Up0 }
  }
}

object HApplyImplicits extends HApplyImplicits

trait HApplyImplicits extends HApplies {

  // HList Apply
  implicit def HListHApply[HA <: HList, HT <: HList, F <: Poly, OutA <: HList, OutT <: HList, Out0 <: HList](
    implicit mapper  : Mapper.Aux[F, HA, OutA],
             tailAp  : HApply.Aux[HA, HT, OutT],
             prepend : Prepend.Aux[OutA, OutT, Out0]
  ) = new HApply[HA, F :: HT] {
    type Up = HList
    type Out = Out0

    def ap(ha: HA)(fs: => F :: HT) = prepend(mapper(ha), tailAp.ap(ha)(fs.tail))
  }

  implicit def HNilHApply[HA] = new HApply[HA, HNil] {
    type Up = HList
    type Out = HNil

    def ap(ha: HA)(fs: => HNil) = HNil
  }

  implicit def HNil2HApply[HA] = new HApply[HA, HNil.type] {
    type Up = HList
    type Out = HNil.type

    def ap(ha: HA)(fs: => HNil.type) = HNil
  }

  // List Apply
  implicit def ListHApply[A, F <: Poly](implicit c: Case1[F, A]) = new HApply[List[A], List[F]] {
    type Up = List[_]
    type Out = List[c.Result]

    def ap(l: List[A])(fs: => List[F]) = fs flatMap { f => l map { a => c(a) } }
  }
}

trait HFunctors {
  trait HFunctor[HA, F <: Poly] {
    type Out

    def map(ha: HA)(f: F): Out
  }
}

object HFunctorImplicits extends HFunctorImplicits

trait HFunctorImplicits extends HFunctors {

  // WARNING: this one shouldn't be needed but apparently scalac requires it for HNil value...
  implicit def HNilHFunctor[F <: Poly](implicit mapper: Mapper[F, HNil]) = new HFunctor[HNil.type, F] {
    type Out = mapper.Out

    def map(l: HNil.type)(f: F) = mapper(l)
  }

  implicit def HListHFunctor[HA <: HList, F <: Poly](implicit mapper: Mapper[F, HA]) = new HFunctor[HA, F] {
    type Out = mapper.Out

    def map(l: HA)(f: F) = mapper(l)
  }

  implicit def ListHFunctor[A, F <: Poly](implicit c: Case1[F, A]) = new HFunctor[List[A], F] {
    type Out = List[c.Result]

    def map(l: List[A])(f: F) = l map { a => c(a) }
  }

  implicit def OptionHFunctor[A, F <: Poly](implicit c: Case1[F, A]) = new HFunctor[Option[A], F] {
    type Out = Option[c.Result]

    def map(l: Option[A])(f: F) = l map { a => c(a) }
  }

  implicit def SizedHFunctor[L, F <: Poly, E, N <: Nat, E1, L1](
    implicit convL : L => GenTraversableLike[E, L],
             c     : Case1.Aux[F, E, E1],
             cbf   : CanBuildFrom[L, E1, L1]
  ) = new HFunctor[Sized[L, N], F] {
    type Out = Sized[L1, N]

    def map(l: Sized[L, N])(f: F) = Sized.wrap[L1, N](l.unsized map { e => c(e) })
  }

}


trait HMonoids {
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

}

trait HMonoidImplicits extends HMonoids {
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
             cbf   : CanBuildFrom[A, E0, B],
             hz    : HSemiGroup[A, B] { type Real = B },
             sum   : Sum.Aux[AN, BN, CN]
  ) = new HSemiGroup[Sized[A, AN], Sized[B, BN]] {

    type Real = Sized[B, Nat]
    type Out = Sized[B, CN]

    def append(a: Sized[A, AN], b: Sized[B, BN]): Out =
      Sized.wrap[B, CN](a.unsized ++ b.unsized)
  }

  implicit def SizedZeroHSemiGroup[A, B, E, E0 >: E, AN <: Nat, BN <: Nat, CN <: Nat](
    implicit convA : A => GenTraversableLike[E, A],
             hz    : HZero { type Real = Sized[A, Nat]; type Zero = Sized[B, BN] }
  ) = new HSemiGroup[Sized[A, AN], Sized[B, BN]] {

    type Real = Sized[A, Nat]
    type Out = Sized[A, AN]

    def append(a: Sized[A, AN], b: Sized[B, BN]): Out = a
  }


}