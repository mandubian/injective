import shapeless._

sealed abstract class Node[R[_, _], A, B] {
  def toDigit(): Digit[R, A, B]
}

object Node {
  case class Node2[R[_, _], A, B, C](a1: R[A, B], a2: R[B, C]) extends Node[R, A, C] {
    def toDigit() = Digit.Two(a1, a2)
  }

  case class Node3[R[_, _], A, B, C, D](a1: R[A, B], a2: R[B, C], a3: R[C, D]) extends Node[R, A, D]{
    def toDigit() = Digit.Three(a1, a2, a3)
  }

}

sealed abstract class Digit[R[_, _], A, B] {
  def toList = Digit.toList(this)
}

object Digit {
  case class One[R[_, _], A, B](a1: R[A, B]) extends Digit[R, A, B]
  case class Two[R[_, _], A, B, C](a1: R[A, B], a2: R[B, C]) extends Digit[R, A, C]
  case class Three[R[_, _], A, B, C, D](a1: R[A, B], a2: R[B, C], a3: R[C, D]) extends Digit[R, A, D]
  case class Four[R[_, _], A, B, C, D, E](a1: R[A, B], a2: R[B, C], a3: R[C, D], a4: R[D, E]) extends Digit[R, A, E]

  def toTree[R[_, _], A, B](d: Digit[R, A, B]): TFingerTree[R, A, B] = {
    type TNode[A, B] = ({ type N[U, V] = Node[R, U, V] })#N[A, B]

    d match {
      case t:One[R, A, B]           => TFingerTree.single(t.a1)
      case t:Two[R, A, u, B]        => TFingerTree.deep(One(t.a1), TFingerTree.empty[TNode, u](), One(t.a2))
      case t:Three[R, A, u, v, B]   => TFingerTree.deep(Two(t.a1, t.a2), TFingerTree.empty[TNode, v](), One(t.a3))
      case t:Four[R, A, u, v, w, B] => TFingerTree.deep(Two(t.a1, t.a2), TFingerTree.empty[TNode, v](), Two(t.a3, t.a4))
    }
  }

  def appendd[R[_, _], A, B, C](d1: Digit[R, A, B], d2: Digit[R, B, C]): Digit[R, A, C] = (d1, d2) match {
    case (One(d1)          , One(d2))           => Two(d1, d2)
    case (One(d1)          , Two(d2, d3))       => Three(d1, d2, d3)
    case (Two(d1, d2)      , One(d3))           => Three(d1, d2, d3)
    case (One(d1)          , Three(d2, d3, d4)) => Four(d1, d2, d3, d4)
    case (Two(d1, d2)      , Two(d3, d4))       => Four(d1, d2, d3, d4)
    case (Three(d1, d2, d3), One(d4))           => Four(d1, d2, d3, d4)
    case _ => sys.error("impossible case")
  }

  import ZList._

  def fromList[R[_, _], A, B](l: ZList[R, A, B]): Digit[R, A, B] = l match {
    case h1 ::: t => t match {
      case _:ZNil[R, B] => One(h1)
      case h2 ::: t2 => t2 match {
        case _:ZNil[R, B] => Two(h1, h2)
        case h3 ::: t3 => t3 match {
          case _:ZNil[R, B] => Three(h1, h2, h3)
          case h4 ::: t5 => t5 match {
            case _:ZNil[R, B] => Four(h1, h2, h3, h4)
            case _ => sys.error("Unmanaged Too Long List")
          }
        }
      }
    }
    case _ => sys.error("impossible case")
  }

  def toList[R[_, _], A, B](d: Digit[R, A, B]) = d match {
    case One(a1)              => :::(a1, ZNil[R, B]())
    case Two(a1, a2)          => :::(a1, :::(a2, ZNil[R, B]()))
    case Three(a1, a2, a3)    => :::(a1, :::(a2, :::(a3, ZNil[R, B]())))
    case Four(a1, a2, a3, a4) => :::(a1, :::(a2, :::(a3, :::(a4, ZNil[R, B]()))))
  }

}


sealed abstract class ZList[R[_, _], A, B] {
  import ZList._

  def :::[AA](pr: => R[AA, A]): ZList[R, AA, B] = ZList.:::[R, AA, A, B](pr, this)

  def append[C](other: => ZList[R, B, C]): ZList[R, A, C] = {
    this match {
      case _:ZNil[R, B] => other.asInstanceOf[ZList[R, A, C]]
      case (h ::: t) => h ::: (t append other)
    }
  }
}

object ZList {
  case class ZNil[R[_, _], A]() extends ZList[R, A, A]

  case class :::[R[_, _], A, B, C](head: R[A, B], tail: ZList[R, B, C]) extends ZList[R, A, C]
}

sealed abstract class TFingerTree[R[_, _], A, B]

object TFingerTree {
  import Digit._
  import Node._
  import TViewl._
  import ZList._

  case class Empty[R[_, _], A]() extends TFingerTree[R, A, A]

  case class Single[R[_, _], A, B](a: R[A, B]) extends TFingerTree[R, A, B]

  case class Deep[R[_, _], A, B, C, D](
    prefix: Digit[R, A, B],
    middle: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    suffix: Digit[R, C, D]
  ) extends TFingerTree[R, A, D]

  def empty[R[_, _], A]() = new Empty[R, A]()

  def single[R[_, _], A, B](a: => R[A, B]) = new Single[R, A, B](a)

  def deep[R[_, _], A, B, C, D](
    prefix: Digit[R, A, B],
    middle: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    suffix: Digit[R, C, D]
  ) = {
    new Deep[R, A, B, C, D](prefix, middle, suffix)
  }

  def prepend[R[_, _], A, B, C](a: R[A, B], tree: TFingerTree[R, B, C]): TFingerTree[R, A, C] = {
    type TNode[A, B] = ({ type N[U, V] = Node[R, U, V] })#N[A, B]

    tree match {
      case t: Empty[R, C] => single(a)
      case t: Single[R, B, C] => deep(One(a), empty[TNode, B](), One(t.a))
      case t: Deep[R, B, u, v, C] => t.prefix match {
        case f: Four[R, B, u1, u2, u3, u4] => deep(
          Two(a, f.a1),
          prepend[TNode, u1, u4, v](
            Node3(f.a2, f.a3, f.a4),
            t.middle
          ),
          t.suffix
        )

        case _ => deep(Digit.appendd(One(a), t.prefix), t.middle, t.suffix)

      }
    }
  }

  def append[R[_, _], A, B, C](tree: TFingerTree[R, A, B], a: R[B, C]): TFingerTree[R, A, C] = {
    type TNode[A, B] = ({ type N[U, V] = Node[R, U, V] })#N[A, B]

    tree match {
      case t: Empty[R, B] => single(a)
      case t: Single[R, A, B] => deep(One(t.a), empty[TNode, B](), One(a))
      case t: Deep[R, A, u, v, B] => t.suffix match {
        case f: Four[R, u1, u2, u3, u4, B] => deep(
          t.prefix,
          append[TNode, u, u1, u4](
            t.middle,
            Node3(f.a1, f.a2, f.a3)
          ),
          Two[R, u4, B, C](f.a4, a)
        )

        case _ => deep(t.prefix, t.middle, Digit.appendd(t.suffix, One(a)))

      }
    }
  }

  def addAllL[R[_, _], A, B, C, D](l: ZList[R, A, B], tree: => TFingerTree[R, B, C]): TFingerTree[R, A, C] = {
    l match {
      case _:ZNil[R, B] => tree
      case h ::: t => prepend(h, addAllL(t, tree))
    }
  }

  def addAllR[R[_, _], A, B, C, D](tree: => TFingerTree[R, A, B], l: ZList[R, B, C]): TFingerTree[R, A, C] = {
    l match {
      case _:ZNil[R, B] => tree
      case h ::: t => addAllR(append(tree, h), t)
    }
  }

  def nodes[R[_, _], A, B](l: ZList[R, A, B]): ZList[({ type N[U, V] = Node[R, U, V] })#N, A, B] = {
    type TNode[A, B] = ({ type N[U, V] = Node[R, U, V] })#N[A, B]

    l match {
      case h1 ::: t1 => t1 match {
        case h2 ::: t2 => t2 match {
          case _:ZNil[R, B] => Node2(h1, h2) ::: ZNil[TNode, B]()
          case h3 ::: t3 => t3 match {
            case _:ZNil[R, B] => Node3(h1, h2, h3) ::: ZNil[TNode, B]()
            case h4 ::: t4 => t4 match {
              case _:ZNil[R, B] => Node2(h1, h2) ::: Node2(h3, h4) ::: ZNil[TNode, B]()
              case _ => Node3(h1, h2, h3) ::: nodes(t3)
            }
          }
        }
        case _ => sys.error("Unmanaged Case")
      }
      case _ => sys.error("Unmanaged Case")
    }
  }

  def app3[R[_, _], A, B, C, D](
    t1: TFingerTree[R, A, B],
    l: => ZList[R, B, C],
    t2: => TFingerTree[R, C, D]
  ): TFingerTree[R, A, D] = {
    type TNode[A, B] = ({ type N[U, V] = Node[R, U, V] })#N[A, B]

    t1 match {
      case _:Empty[R, A]            => addAllL(l, t2)

      case t11: Single[R, A, B]     => prepend(t11.a, addAllL(l, t2))

      case t11: Deep[R, A, u, v, B] =>
        t2 match {
          case _:Empty[R, C]              => addAllR(t1, l)
          case t22: Single[R, C, D]       => append(addAllR(t1, l), t22.a)
          case t22: Deep[R, C, w, x, D]   => deep(
            t11.prefix,
            app3[TNode, u, v, w, x](
              t11.middle,
              nodes(
                Digit.toList(t11.suffix) append (l append Digit.toList(t22.prefix))
              ),
              t22.middle
            ),
            t22.suffix
          )
        }
    }
  }

  def app2[R[_, _], A, B, C](
    t1: TFingerTree[R, A, B],
    t2: => TFingerTree[R, B, C]
  ): TFingerTree[R, A, C] = {
    t1 match {
      case _:Empty[R, A] => t2
      case t11: Single[R, A, B] => prepend(t11.a, t2)
      case t11: Deep[R, A, u, v, B] => t2 match {
        case _:Empty[R, B] => t1
        case t22: Single[R, B, C] => append(t1, t22.a)
        case t22: Deep[R, B, w, x, C] => deep(
          t11.prefix,
          app3[({ type N[U, V] = Node[R, U, V] })#N, u, v, w, x](
            t11.middle,
            nodes( Digit.toList(t11.suffix) append Digit.toList(t22.prefix) ),
            t22.middle
          ),
          t22.suffix
        )
      }
    }
  }

  def deepL[R[_, _], A, B, C, D](
    pr: ZList[R, A, B],
    m:  => TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    sf: Digit[R, C, D]
  )(implicit TS: TSequence[TFingerTree]): TFingerTree[R, A, D] = {
    import TViewl._

    type TNode[A, B] = ({ type N[U, V] = Node[R, U, V] })#N[A, B]

    pr match {
      case _:ZNil[R, A] =>
        TS.tviewl[TNode, B, C](m) match {
          case EmptyL() =>
            Digit.toTree(sf).asInstanceOf[TFingerTree[R, A, D]]

          case l:LeafL[TFingerTree, TNode, B, u, C] =>
            deep(l.head().toDigit, l.tail(), sf)
        }

      case _ => deep(Digit.fromList(pr), m, sf)
    }
  }

  implicit object TFingerTreeSeq extends TSequence[TFingerTree] {
    def tempty[C[_, _], X]: TFingerTree[C, X, X] = TFingerTree.empty()

    def tsingleton[C[_, _], X, Y](c: => C[X, Y]): TFingerTree[C, X, Y] = TFingerTree.single(c)

    def tappend[C[_, _], X, Y, Z](a: TFingerTree[C, X, Y], b: => TFingerTree[C, Y, Z]): TFingerTree[C, X, Z] = {
      app2(a, b)
    }

    def tviewl[C[_, _], X, Y](s: TFingerTree[C, X, Y]): TViewl[TFingerTree, C, X, Y] = {
      s match {
        case _:Empty[C, X] => TViewl.EmptyL[TFingerTree, C, X]()
        case t: Single[C, X, Y] => TViewl.LeafL[TFingerTree, C, X, Y, Y](() => t.a, () => TFingerTree.empty())
        case t: Deep[C, X, u, v, Y] =>
          Digit.toList(t.prefix) match {
            case hh ::: tt =>
              TViewl.LeafL(() => hh, () => TFingerTree.deepL(tt, t.middle, t.suffix))
          }
      }
    }
  }

}


