
import shapeless._
import ops.hlist.Prepend
import ops.nat.{Sum, Pred, ToInt}

object ShapelessExt {

  trait ~~>[F[_, _], R] extends Poly1 {
    def apply[A, B](f : F[A, B]) : R
    implicit def caseUniv[A, B]: Case.Aux[F[A, B], R] = at[F[A, B]](apply(_))
  }


  trait HPrepend[H, P <: H, S <: H] extends DepFn2[P, S] { type Out <: H }


  trait HMonoid[H, Z <: H] {
    def zero: Z
    def append[A <: H, B <: H, R <: H](f1: A, f2: B)(implicit prepend: HPrepend[H, A, B]): prepend.Out = prepend(f1, f2)
  }

  implicit def HListHPrepend[A <: HList, B <: HList](implicit prepend: Prepend[A, B]): HPrepend[HList, A, B] = new HPrepend[HList, A, B] {
    type Out = prepend.Out

    def apply(prefix : A, suffix : B): Out = prepend(prefix, suffix)
  }

  implicit def HListHMonoid(implicit prepend: Prepend[A, B]) = new HMonoid[HList, HNil] {
    def zero = HNil

    implicit val prepender: HPrepend[HList, A, B] = new HPrepend[HList, A, B] {
      type Out = prepend.Out

      def apply(prefix : A, suffix : B): Out = prepend(prefix, suffix)
    }
  }

  implicit def NatHPrepend[A <: Nat, B <: Nat, C <: Nat]
    (implicit sum: Sum.Aux[A, B, C], pred: Pred[C]): HPrepend[Nat, A, B] = new HPrepend[Nat, A, B] {
    type Out = Succ[pred.Out]

    def apply(a : A, b : B): Out = new Succ[pred.Out]
  }

  implicit def NatHMonoid = new HMonoid[Nat, _0] {
    def zero = Nat._0
  }
}