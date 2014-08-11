
sealed abstract class TFingerTree[R[_, _], A, B]

object TFingerTree {
  import Digit._

  class Empty[R[_, _], A]() extends TFingerTree[R, A, A]

  class Single[R[_, _], A, B](a: => R[A, B]) extends TFingerTree[R, A, B]

  class Deep[R[_, _], A, B, C, D](
    left: Digit[R, A, B],
    focus: => TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    right: Digit[R, C, D]
  ) extends TFingerTree[R, A, D]

  def empty[R[_, _], A] = new Empty[R, A]()

  def single[R[_, _], A, B](a: => R[A, B]) = new Single[R, A, B](a)

  def deep[R[_, _], A, B, C, D](
    left: Digit[R, A, B],
    focus: => TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    right: Digit[R, C, D]
  ) = new Deep[R, A, B, C, D](left, focus, right)

  def toTree[R[_, _], A, B](digit: One[R, A, B]): TFingerTree[R, A, B] = single(digit.a1)
  def toTree[R[_, _], A, B, C](digit: Two[R, A, B, C]): TFingerTree[R, A, C] =
    deep[R, A, B, B, C](One(digit.a1), empty[({ type N[U, V] = Node[R, U, V] })#N, B], One(digit.a2))
  def toTree[R[_, _], A, B, C, D](digit: Three[R, A, B, C, D]): TFingerTree[R, A, D] =
    deep[R, A, C, C, D](Two(digit.a1, digit.a2), empty[({ type N[U, V] = Node[R, U, V] })#N, C], One(digit.a3))
  def toTree[R[_, _], A, B, C, D, E](digit: Four[R, A, B, C, D, E]): TFingerTree[R, A, E] =
    deep[R, A, C, C, E](Two(digit.a1, digit.a2), empty[({ type N[U, V] = Node[R, U, V] })#N, C], Two(digit.a3, digit.a4))

  def appendd[R[_, _], A, B, C](d1: One[R, A, B], d2: One[R, B, C]): Digit[R, A, C] = Two(d1.a1, d2.a1)
  def appendd[R[_, _], A, B, C, D](d1: One[R, A, B], d2: Two[R, B, C, D]): Digit[R, A, D] = Three(d1.a1, d2.a1, d2.a2)
  def appendd[R[_, _], A, B, C, D](d1: Two[R, A, B, C], d2: One[R, C, D]): Digit[R, A, D] = Three(d1.a1, d1.a2, d2.a1)
  def appendd[R[_, _], A, B, C, D, E](d1: One[R, A, B], d2: Three[R, B, C, D, E]): Digit[R, A, E] = Four(d1.a1, d2.a1, d2.a2, d2.a3)
  def appendd[R[_, _], A, B, C, D, E](d1: Two[R, A, B, C], d2: Two[R, C, D, E]): Digit[R, A, E] = Four(d1.a1, d1.a2, d2.a1, d2.a2)
  def appendd[R[_, _], A, B, C, D, E](d1: Three[R, A, B, C, D], d2: One[R, D, E]): Digit[R, A, E] = Four(d1.a1, d1.a2, d1.a3, d2.a1)

  //def tappend[C[_, _], X, Y, Z](a: TFingerTree[C, X, Y], b: TFingerTree[C, Y, Z]): TFingerTree[C, X, Z]

  import shapeless.{HList, ::, HNil}
  def toList[R[_, _], A, B](d: One[R, A, B]) = d.a1 :: HNil
  def toList[R[_, _], A, B, C](d: Two[R, A, B, C]) = d.a1 :: d.a2 :: HNil
  def toList[R[_, _], A, B, C, D](d: Three[R, A, B, C, D]) = d.a1 :: d.a2 :: d.a3 :: HNil
  def toList[R[_, _], A, B, C, D, E](d: Four[R, A, B, C, D, E]) = d.a1 :: d.a2 :: d.a3 :: d.a4 :: HNil

  def fromList[R[_, _], A, B, C, D, E](l: R[A, B] :: R[B, C] :: R[C, D] :: R[D, E] :: HNil) = Four(l(0), l(1), l(2), l(3))
  def fromList[R[_, _], A, B, C, D](l: R[A, B] :: R[B, C] :: R[C, D] :: HNil) = Three(l(0), l(1), l(2))
  def fromList[R[_, _], A, B, C](l: R[A, B] :: R[B, C] :: HNil) = Two(l(0), l(1))
  def fromList[R[_, _], A, B](l: R[A, B] :: HNil) = One(l(0))


  implicit object TFingerTreeTSequence extends TSequence[TFingerTree] {
    def tempty[C[_, _], X]: TFingerTree[C, X, X] = empty[C, X]
    def tsingleton[C[_, _], X, Y](c: C[X, Y]): TFingerTree[C, X, Y] = single[C, X, Y](c)

    def tappend[C[_, _], X, Y, Z](a: TFingerTree[C, X, Y], b: TFingerTree[C, Y, Z]): TFingerTree[C, X, Z] = {
      (a, b) match {
        case (a: Two[C, X, _, Y], b: Two[C, Y, _, Z]) => toTree(appendd(a, b))
      }
    }

  }
}


sealed abstract class Node[R[_, _], A, B]

object Node {
  class Node2[R[_, _], A, B, C](a1: => R[A, B], a2: => R[B, C]) extends Node[R, A, C]
  class Node3[R[_, _], A, B, C, D](a1: => R[A, B], a2: => R[B, C], a3: => R[C, D]) extends Node[R, A, D]
}

sealed abstract class Digit[R[_, _], A, B]

object Digit {
  case class One[R[_, _], A, B](a1: R[A, B]) extends Digit[R, A, B]
  case class Two[R[_, _], A, B, C](a1: R[A, B], a2: R[B, C]) extends Digit[R, A, C]
  case class Three[R[_, _], A, B, C, D](a1: R[A, B], a2: R[B, C], a3: R[C, D]) extends Digit[R, A, D]
  case class Four[R[_, _], A, B, C, D, E](a1: R[A, B], a2: R[B, C], a3: R[C, D], a4: R[D, E]) extends Digit[R, A, E]
}

