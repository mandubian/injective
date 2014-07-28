/** A universally quantified value */
trait Forall2[P[_, _]] {
  def apply[A, B]: P[A, B]
}

object Forall2 extends Foralls2

trait Foralls2 {
  /** Universal quantification by doubly negating an existential. */
  type Not[A] = A => Nothing
  type DNE[P[_, _]] = Not[P[A, B]] forSome {type A ; type B}
  type CPS[P[_, _]] = Not[DNE[P]]

  /** Construct a universal quantifier by continuation-passing. */
  def apply[P[_, _]](p: CPS[P]): Forall2[P] = new Forall2[P] {
    def apply[A, B]: P[A, B] = {
      case class Control(arg: P[A, B]) extends Throwable
      try {
        p((arg: P[A, B]) => throw new Control(arg))
      } catch {
        case Control(arg) => arg
      }
    }
  }
}

// type C[A, B] = Map[A, B] => Int
// val f = Forall2((k: Forall2.DNE[C]) => k(m => m.size))

// scala> f.apply(Map(1 -> 2, 3 -> 4))
// res44: Int = 2

// scala> f.apply(Map(1 -> 2, 3 -> 4, 5 -> 6))
// res45: Int = 3
