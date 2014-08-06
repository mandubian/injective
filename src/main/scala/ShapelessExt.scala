
import shapeless._
import poly.Case1
import ops.hlist.{Prepend, Mapper, FlatMapper}
import ops.nat.{Sum, Pred, ToInt, Prod}
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


trait HMonads extends HPoints {

  trait HMonad[HA, F] extends HPoint {
    type Up
    type Out

    def bind(fa: HA)(f: F): Out
  }

}

object HMonadImplicits
  extends HMonadImplicits
  with HPointerImplicits

trait HMonadImplicits extends HMonads {

  implicit def HListHMonad[HA <: HList, F <: Poly](
    implicit flatMapper: FlatMapper[F, HA],
             p: UPointer[HList]
  ) = new HMonad[HA, F] {
    type Up = HList
    type Out = flatMapper.Out

    val pointer = p
    def bind(l: HA)(f: F) = flatMapper(l)
  }

  implicit def HNilHMonad[F <: Poly](
    implicit flatMapper: FlatMapper[F, HNil],
             p: UPointer[HList]
  ) = new HMonad[HNil.type, F] {
    type Up = HList
    type Out = flatMapper.Out

    val pointer = p
    def bind(l: HNil.type)(f: F) = flatMapper(l)
  }

  implicit def ListHMonad[A, F <: Poly, B](
    implicit c: Case1.Aux[F, A, List[B]],
             p: UPointer[List[_]]
  ) = new HMonad[List[A], F] {
    type Up = List[_]
    type Out = List[B]

    val pointer = p

    def bind(l: List[A])(f: F) = l flatMap { a => c(a) }
  }

  implicit def OptionHMonad[A, F <: Poly, B](
    implicit c: Case1.Aux[F, A, Option[B]],
             p: UPointer[Option[_]]
  ) = new HMonad[Option[A], F] {
    type Up = Option[_]
    type Out = Option[B]

    val pointer = p

    def bind(l: Option[A])(f: F) = l flatMap { a => c(a) }
  }

  implicit def OptionSomeHMonad[A, F <: Poly, B](
    implicit c: Case1.Aux[F, A, Option[B]],
             p: UPointer[Option[_]]
  ) = new HMonad[Some[A], F] {
    type Up = Option[_]
    type Out = Option[B]

    val pointer = p

    def bind(l: Some[A])(f: F) = l flatMap { a => c(a) }
  }

  implicit def OptionNoneHMonad[A, F <: Poly, B](
    implicit c: Case1.Aux[F, A, Option[B]],
             p: UPointer[Option[_]]
  ) = new HMonad[None.type, F] {
    type Up = Option[_]
    type Out = None.type

    val pointer = p

    def bind(l: None.type)(f: F) = None
  }


  implicit def SizedHMonad[L, N <: Nat, F <: Poly, E, U, S1, L1, N1 <: Nat, L2, E1, L3](
    implicit convL : L => GenTraversableLike[E, L],
             pp    : HPointer.Aux[E, U, L],
             c     : Case1.Aux[F, E, Sized[L1, N1]],
             convL1: L1 => GenTraversableLike[E1, L1],
             cbf2  : CanBuildFrom[L, E1, L1],
             p     : UPointer[Sized[U, Nat._1]],
             sum   : Sum[N, N1],
             prod  : Prod[N, N1]
  ) = new HMonad[Sized[L, N], F] {
    type Up = Sized[U, Nat._1]
    type Out = Sized[L1, prod.Out]

    val pointer = p

    def bind(l: Sized[L, N])(f: F) = {
      val ls = l.unsized flatMap { e => c(e).asInstanceOf[Sized[L1, N1]].unsized }
      Sized.wrap[L1, prod.Out](ls)
    }
  }
}

trait HApplicatives 
  extends HApplies
  with HPoints
  with HFunctors {

  trait HApplicative[HA, HF] extends HApply[HA, HF] with HPoint

  trait Rel[HA, Up]

  object Rel {

    implicit def HListRel[HA <: HList] = new Rel[HA, HList] {}

    implicit def ListRel[A, F] = new Rel[List[A], List[_]] {}
  }

}

object HApplicativeImplicits extends HApplicativeImplicits

trait HApplicativeImplicits
  extends HApplicatives
  with HApplyImplicits
  with HPointerImplicits {

  implicit def happlicative[HA, HF, U](
    implicit happ: HApply.Up[HA, HF, U],
             p: UPointer[U]
  ) = new HApplicative[HA, HF] {
    type Out = happ.Out
    type Up = happ.Up

    val pointer = p

    def ap(ha: HA)(fs: => HF) = happ.ap(ha)(fs)
  }

  implicit def hfunctor[HA, F <: Poly, UP, FO](
    implicit  rel: Rel[HA, UP],
              hpointer: HPointer.Aux[F, UP, FO],
              happ: HApplicative[HA, FO]
  ) = new HFunctor[HA, F] {
    type Out = happ.Out

    def map(ha: HA)(f: F): Out = happ.ap(ha)(hpointer(f))
  }
}

trait HPoints extends HPointers {

  trait HPoint {
    type Up

    val pointer: UPointer[Up]

    def point[A, O <: Up](a: A): pointer.Out[A] = pointer(a)
  }
}

trait HPointers {
  trait UPointer[Up] {
    type Out[A] <: Up
    def apply[A](a: A): Out[A]
  }

  trait HPointer[H] extends DepFn1[H] {
    type Up
    type Out
  }

  object HPointer {
    type Aux[H, U, O] = HPointer[H] {
      type Up = U
      type Out = O
    }
  }

}

object HPointerImplicits extends HPointerImplicits

trait HPointerImplicits extends HPointers {
  implicit def HListHPointer[A] = new HPointer[A] {
    type Up = HList
    type Out = A :: HNil
    def apply(a: A) = a :: HNil
  }

  implicit def HListUPointer = new UPointer[HList] {
    type Out[A] = A :: HNil
    def apply[A](a: A) = a :: HNil
  }

  implicit def ListHPointer[A] = new HPointer[A] {
    type Up = List[_]
    type Out = List[A]
    def apply(a: A) = List(a)
  }

  implicit def ListUPointer = new UPointer[List[_]] {
    type Out[A] = List[A]
    def apply[A](a: A) = List(a)
  }

  implicit def OptionUPointer = new UPointer[Option[_]] {
    type Out[A] = Option[A]
    def apply[A](a: A) = Some(a)
  }

  implicit def SizedUPointer2[C[_]](implicit cup: UPointer[C[_]]) =
    new UPointer[Sized[C[_], Nat._1]] {
      type Out[A] = Sized[cup.Out[A], Nat._1]
      def apply[A](a: A) = Sized.wrap[cup.Out[A], Nat._1](cup(a))
    }

  implicit def SizedUPointer[C](implicit cup: UPointer[C]) =
    new UPointer[Sized[C, Nat._1]] {
      type Out[A] = Sized[cup.Out[A], Nat._1]
      def apply[A](a: A) = Sized.wrap[cup.Out[A], Nat._1](cup(a))
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

object HMonoidImplicits extends HMonoidImplicits

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