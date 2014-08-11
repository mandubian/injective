
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

  trait HMonad[HA, F] {
    type Out

    def bind(fa: HA)(f: F): Out
  }

  object HMonad extends HPoint {

    def apply[HA, HF](implicit hm: HMonad[HA, HF]) = hm

  }
}

object HMonadImplicits
  extends HMonadImplicits
  with HPointerImplicits

trait HMonadImplicits extends HMonads with HApplicatives {

  implicit def HListHMonad[HA <: HList, F <: Poly](
    implicit flatMapper: FlatMapper[F, HA]
  ) = new HMonad[HA, F] {
    type Out = flatMapper.Out

    def bind(l: HA)(f: F) = flatMapper(l)
  }

  implicit def HNilHMonad[F <: Poly, P[_]](
    implicit flatMapper: FlatMapper[F, HNil]
  ) = new HMonad[HNil.type, F] {
    type Out = flatMapper.Out

    def bind(l: HNil.type)(f: F) = flatMapper(l)
  }

  implicit def ListHMonad[A, F <: Poly, B, P[_]](
    implicit c: Case1.Aux[F, A, List[B]]
  ) = new HMonad[List[A], F] {
    type Out = List[B]

    def bind(l: List[A])(f: F) = l flatMap { a => c(a) }
  }

  implicit def OptionHMonad[A, F <: Poly, B, P[_]](
    implicit c: Case1.Aux[F, A, Option[B]]
  ) = new HMonad[Option[A], F] {
    type Out = Option[B]

    def bind(l: Option[A])(f: F) = l flatMap { a => c(a) }
  }

  implicit def OptionSomeHMonad[A, F <: Poly, B, P[_]](
    implicit c: Case1.Aux[F, A, Option[B]]
  ) = new HMonad[Some[A], F] {
    type Out = Option[B]

    def bind(l: Some[A])(f: F) = l flatMap { a => c(a) }
  }

  implicit def OptionNoneHMonad[A, F <: Poly, B, P[_]](
    implicit c: Case1.Aux[F, A, Option[B]]
  ) = new HMonad[None.type, F] {
    type Out = None.type

    def bind(l: None.type)(f: F) = None
  }


  implicit def SizedHMonad[L, N <: Nat, F <: Poly, E, U, S1, L1, N1 <: Nat, L2, E1, L3](
    implicit convL : L => GenTraversableLike[E, L],
             pp    : HPointer.AuxUO[E, U, L],
             c     : Case1.Aux[F, E, Sized[L1, N1]],
             convL1: L1 => GenTraversableLike[E1, L1],
             cbf2  : CanBuildFrom[L, E1, L1],
             sum   : Sum[N, N1],
             prod  : Prod[N, N1]
  ) = new HMonad[Sized[L, N], F] {

    type Out = Sized[L1, prod.Out]

    def bind(l: Sized[L, N])(f: F) = {
      val ls = l.unsized flatMap { e => c(e).asInstanceOf[Sized[L1, N1]].unsized }
      Sized.wrap[L1, prod.Out](ls)
    }
  }

}

trait HApplicatives
  extends HApplies
  with HFunctors
  with HPoints {

  trait HApplicative[HA, HF] extends HApply[HA, HF]

  object HApplicative extends HPoint {

    def apply[HA, HF](implicit hap: HApplicative[HA, HF]) = hap

  }

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

  implicit def happlicative[HA, HF, P[_]](
    implicit happ: HApply[HA, HF]
  ) = new HApplicative[HA, HF] {
    type Out = happ.Out

    def ap(ha: => HA)(fs: => HF) = happ.ap(ha)(fs)
  }

  implicit def hfunctor[HA, F <: Poly, UP, FO](
    implicit  rel: Rel[HA, UP],
              hpointer: HPointer.AuxUO[F, UP, FO],
              happ: HApplicative[HA, FO]
  ) = new HFunctor[HA, F] {
    type Out = happ.Out

    def map(ha: HA)(f: F): Out = happ.ap(ha)(hpointer(f))
  }
}

trait HPoints extends HPointers {

  trait HPoint {

    // The Point Poly (representing Monad.return in types)
    def pointF[HA, A](implicit p: TPointer[HA, A]) = Poly {
      def apply(a: A) = p(a)
    }

  }
}

trait HPointers {
  trait HPointer[H] extends DepFn1[H] {
    type Up
    type Out
  }

  object HPointer {
    type AuxU[H, U] = HPointer[H] {
      type Up = U
    }

    type AuxUO[H, U, O] = HPointer[H] {
      type Up = U
      type Out = O
    }
  }

  trait TPointer[HA, A] extends DepFn1[A] { type Out = HA }

  object TPointer {
    type Aux[HA, A, Out0] = TPointer[HA, A] { type Out = Out0 }
    def apply[HA, A](implicit tp: TPointer[HA, A]): Aux[HA, A, tp.Out] = tp    
  }
}

object HPointerImplicits extends HPointerImplicits

trait HPointerImplicits extends HPointers {
  implicit def HListHPointer[A] = new HPointer[A] {
    type Up = HList
    type Out = A :: HNil
    def apply(a: A) = a :: HNil
  }

  implicit def HListTPointer[A] = new TPointer[A :: HNil, A] {
    def apply(a: A) = a :: HNil
  }

  implicit def ListHPointer[A] = new HPointer[A] {
    type Up = List[_]
    type Out = List[A]
    def apply(a: A) = List(a)
  }

  implicit def ListTPointer[A] = new TPointer[List[A], A] {
    def apply(a: A) = List(a)
  }

  implicit def OptionTPointer[A] = new TPointer[Option[A], A] {
    def apply(a: A) = Option(a)
  }

  implicit def SizedTPointer[R[_], A](implicit tp: TPointer[R[A], A]) = 
    new TPointer[Sized[R[A], Nat._1], A] {
      def apply(a: A) = Sized.wrap[R[A], Nat._1](tp(a))
    }

}



trait HApplies {

  trait HApplier[HF, In] extends DepFn2[In, HF] { type Out }

  trait HApply[HA, HF]{
    type Up
    type Out

    def ap(ha: => HA)(fs: => HF): Out
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

    def ap(ha: => HA)(fs: => F :: HT) = prepend(mapper(ha), tailAp.ap(ha)(fs.tail))
  }

  implicit def HNilHApply[HA] = new HApply[HA, HNil] {
    type Up = HList
    type Out = HNil

    def ap(ha: => HA)(fs: => HNil) = HNil
  }

  implicit def HNil2HApply[HA] = new HApply[HA, HNil.type] {
    type Up = HList
    type Out = HNil.type

    def ap(ha: => HA)(fs: => HNil.type) = HNil
  }

  // List Apply
  implicit def ListHApply[A, F <: Poly](implicit c: Case1[F, A]) = new HApply[List[A], List[F]] {
    type Up = List[_]
    type Out = List[c.Result]

    def ap(l: => List[A])(fs: => List[F]) = fs flatMap { f => l map { a => c(a) } }
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



  // implicit def hfunctor[HA, F <: Poly](
  //   implicit //hp  : Rel[HA, U],
  //            //up  : UPointer[U],
  //            hm  : HMonad[HA, F]
  // ) = new HFunctor[HA, F] {
  //   type Out = hm.Out

  //   def map(ha: HA)(f: F): Out = hm.bind(ha)(f)
  // }

  // implicit def happlicative[HA, HF, F <: Poly, U, P <: Poly](
  //   implicit hp     : Rel[HA, U],
  //            up     : UPointer[U],
  //            pm     : toPolyHA.Case.Aux[HA, P],
  //            hm     : HMonad[HF, P]
  // ) = new HApplicative[HA, HF] {
  //   type Up = hm.Up
  //   type Out = hm.Out

  //   val pointer = hm.pointer

  //   def ap(ha: => HA)(fs: => HF): Out = {
  //     lazy val lha = ha

  //     hm.bind(fs)(pm(ha))
  //   }
  // }

  // object toPolyHA extends Poly1 {
  //   implicit def caseHA[HA, U](
  //     implicit hp      : Rel[HA, U],
  //              upointer: UPointer[U]
  //   ) = at[HA]{ ha => polyHA(ha) }
  // }

  // def polyHA[HA, U](ha: HA)(implicit 
  //   hp      : Rel[HA, U],
  //   upointer: UPointer[U]
  // ) = new Poly1 {
  //   implicit def caseP[P <: Poly](implicit fun: HFunctor[HA, P]) = 
  //     at[P]{ p => upointer(fun.map(ha)(p)) }
  // }