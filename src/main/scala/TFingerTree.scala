import shapeless._

sealed abstract class Node[R[_, _], A, B]

object Node {
  case class Node2[R[_, _], A, B, C](a1: R[A, B], a2: R[B, C]) extends Node[R, A, C]
  case class Node3[R[_, _], A, B, C, D](a1: R[A, B], a2: R[B, C], a3: R[C, D]) extends Node[R, A, D]
}

sealed abstract class Digit[R[_, _], A, B]

object Digit {
  // TODO ADD FCT BY_VAL TO BY_NAME
  case class One[R[_, _], A, B](a1: R[A, B]) extends Digit[R, A, B]
  case class Two[R[_, _], A, B, C](a1: R[A, B], a2: R[B, C]) extends Digit[R, A, C]
  case class Three[R[_, _], A, B, C, D](a1: R[A, B], a2: R[B, C], a3: R[C, D]) extends Digit[R, A, D]
  case class Four[R[_, _], A, B, C, D, E](a1: R[A, B], a2: R[B, C], a3: R[C, D], a4: R[D, E]) extends Digit[R, A, E]

  def toTree[R[_, _], A, B](d: Digit[R, A, B]): TFingerTree[R, A, B] = d match {
    case One(a1)                  => TFingerTree.single(a1)
    case t:Two[R, A, u, B]        => TFingerTree.deep(One(t.a1), TFingerTree.empty[({ type N[U, V] = Node[R, U, V] })#N, u](), One(t.a2))
    case t:Three[R, A, u, v, B]   => TFingerTree.deep(Two(t.a1, t.a2), TFingerTree.empty[({ type N[U, V] = Node[R, U, V] })#N, v](), One(t.a3))
    case t:Four[R, A, u, v, w, B]   => TFingerTree.deep(Two(t.a1, t.a2), TFingerTree.empty[({ type N[U, V] = Node[R, U, V] })#N, v](), Two(t.a3, t.a4))
  }

  def appendd[R[_, _], A, B, C](d1: Digit[R, A, B], d2: Digit[R, B, C]): Digit[R, A, C] = (d1, d2) match {
    case (d1: One[R, A, B]        , d2: One[R, B, C])         => Two(d1.a1, d2.a1)
    case (d1: One[R, A, B]        , d2: Two[R, B, u, C])      => Three(d1.a1, d2.a1, d2.a2)
    case (d1: Two[R, A, u, B]     , d2: One[R, B, C])         => Three(d1.a1, d1.a2, d2.a1)
    case (d1: One[R, A, B]        , d2: Three[R, B, u, v, C]) => Four(d1.a1, d2.a1, d2.a2, d2.a3)
    case (d1: Two[R, A, u, B]     , d2: Two[R, B, v, C])      => Four(d1.a1, d1.a2, d2.a1, d2.a2)
    case (d1: Three[R, A, u, v, B], d2: One[R, B, C])         => Four(d1.a1, d1.a2, d1.a3, d2.a1)
    case _ => sys.error("impossible case")
  }

  import ZList._

  def fromList[R[_, _], A, B](l: ZList[R, A, B]): Digit[R, A, B] = l match {
    case h1 ::: t => t match {
      case _:ZNil[R, B] => One(h1)
      case t2 => t2 match {
        case h2 ::: t2 => t2 match {
          case _:ZNil[R, B] => Two(h1, h2)
          case t3 => t3 match {
            case h3 ::: t3 => t3 match {
              case _:ZNil[R, B] => Three(h1, h2, h3)
              case t4 => t4 match {
                case h4 ::: t5 => t5 match {
                  case _:ZNil[R, B] => Four(h1, h2, h3, h4)
                  case _ => sys.error("Unmanaged Too Long List")
                }
                case _ => sys.error("impossible case")
              }
            }
            case _ => sys.error("impossible case")
          }
        }
        case _ => sys.error("impossible case")
      }
    }
    case _ => sys.error("impossible case")
  }

  // def fromList[R[_, _], A, B, C, D, E](l: R[A, B] :: R[B, C] :: R[C, D] :: R[D, E] :: HNil) = Four(l(0), l(1), l(2), l(3))
  // def fromList[R[_, _], A, B, C, D](l: R[A, B] :: R[B, C] :: R[C, D] :: HNil) = Three(l(0), l(1), l(2))
  // def fromList[R[_, _], A, B, C](l: R[A, B] :: R[B, C] :: HNil) = Two(l(0), l(1))
  // def fromList[R[_, _], A, B](l: R[A, B] :: HNil) = One(l(0))

  def toList[R[_, _], A, B](d: Digit[R, A, B]) = d match {
    case One(a1)              => :::(a1, ZNil[R, B]())
    case Two(a1, a2)          => :::(a1, :::(a2, ZNil[R, B]()))
    case Three(a1, a2, a3)    => :::(a1, :::(a2, :::(a3, ZNil[R, B]())))
    case Four(a1, a2, a3, a4) => :::(a1, :::(a2, :::(a3, :::(a4, ZNil[R, B]()))))
  }

}


sealed abstract class ZList[R[_, _], A, B] {
  def :::[AA](pr: R[AA, A]): ZList[R, AA, B] = ZList.:::[R, AA, A, B](pr, this)  
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
    middle: => TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    suffix: Digit[R, C, D]
  ) = new Deep[R, A, B, C, D](prefix, middle, suffix)

  def prepend[R[_, _], A, B, C](a: R[A, B], tree: TFingerTree[R, B, C]): TFingerTree[R, A, C] = {
    tree match {
      case t: Empty[R, C] => single(a)
      case t: Single[R, B, C] => deep(One(a), empty[({ type N[U, V] = Node[R, U, V] })#N, B](), One(t.a))
      case t: Deep[R, B, u, v, C] => t.prefix match {
        case f: Four[R, B, u1, u2, u3, u4] => deep(
          Two[R, A, B, u1](a, f.a1),
          prepend[({ type N[U, V] = Node[R, U, V] })#N, u1, u4, v](
            Node3(f.a2, f.a3, f.a4),
            t.middle.asInstanceOf[TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, u4, v]]
          ),
          t.suffix
        )

        case _ => deep(Digit.appendd(One(a), t.prefix), t.middle, t.suffix)

      }
    }
  }

  def append[R[_, _], A, B, C](tree: TFingerTree[R, A, B], a: R[B, C]): TFingerTree[R, A, C] = {
    tree match {
      case t: Empty[R, B] => single(a)
      case t: Single[R, A, B] => deep(One(t.a), empty[({ type N[U, V] = Node[R, U, V] })#N, B](), One(a))
      case t: Deep[R, A, u, v, B] => t.suffix match {
        case f: Four[R, u1, u2, u3, u4, B] => deep(
          t.prefix,
          append[({ type N[U, V] = Node[R, U, V] })#N, u, u1, u4](
            t.middle.asInstanceOf[TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, u, u1]],
            Node3(f.a1, f.a2, f.a3)
          ),
          Two[R, u4, B, C](f.a4, a)
        )

        case _ => deep(t.prefix, t.middle, Digit.appendd(t.suffix, One(a)))

      }
    }
  }

  def addAllL[R[_, _], A, B, C, D](l: ZList[R, A, B], tree: TFingerTree[R, B, C]): TFingerTree[R, A, C] = {
    l match {
      case _:ZNil[R, B] => tree
      case h ::: t => prepend(h, addAllL(t, tree))
    }
  }

  def addAllR[R[_, _], A, B, C, D](tree: TFingerTree[R, A, B], l: ZList[R, B, C]): TFingerTree[R, A, C] = {
    l match {
      case _:ZNil[R, B] => tree
      case h ::: t => addAllR(append(tree, h), t)
    }
  }

  def nodes[R[_, _], A, B](l: ZList[R, A, B]): ZList[({ type N[U, V] = Node[R, U, V] })#N, A, B] = l match {
    case h1 ::: t => t match {
      case _:ZNil[R, B] => sys.error("Unmanaged Case")
      case t2 => t2 match {
        case h2 ::: t2 => t2 match {
          case _:ZNil[R, B] => Node2(h1, h2) ::: ZNil[({ type N[U, V] = Node[R, U, V] })#N, B]()
          case t3 => t3 match {
            case h3 ::: t3 => t3 match {
              case _:ZNil[R, B] => Node3(h1, h2, h3) ::: ZNil[({ type N[U, V] = Node[R, U, V] })#N, B]
              case t4 => t4 match {
                case h4 ::: t5 => t5 match {
                  case _:ZNil[R, B] => Node2(h1, h2) ::: Node2(h3, h4) ::: ZNil[({ type N[U, V] = Node[R, U, V] })#N, B]
                  case t6 => sys.error("Unmanaged Too Long List")
                }
                case _ => Node3(h1, h2, h3) ::: nodes(t3)
              }
            }
            case _ => sys.error("Unmanaged Case")
          }
        }
        case _ => sys.error("Unmanaged Case")
      }
    }
    case _ => sys.error("Unmanaged Case")
  }

  def app3[R[_, _], A, B, C, D](t1: TFingerTree[R, A, B], l: ZList[R, B, C], t2: TFingerTree[R, C, D]): TFingerTree[R, A, D] = {
    t1 match {
      case _:Empty[R, A] => addAllL(l, t2)
      case t11: Single[R, A, B] => prepend(t11.a, addAllL(l, t2))
      case t11: Deep[R, A, u, v, B] => t2 match {
        case _:Empty[R, C] => addAllR(t1, l)
        case t22: Single[R, C, D] => append(addAllR(t1, l), t22.a)
        case t22: Deep[R, C, w, x, D] => deep(
          t11.prefix,
          app3(
            t11.middle,
            nodes(
              append(
                Digit.toList(t11.suffix),
                append(l, Digit.toList(t22.prefix))
              )
            ),
            t22.middle
          ),
          t11.suffix
        )
      }
    }
  }
}

  // def toList[R[_, _], A, B](d: One[R, A, B]) = d.a1 :: HNil
  // def toList[R[_, _], A, B, C](d: Two[R, A, B, C]) = d.a1 :: d.a2 :: HNil
  // def toList[R[_, _], A, B, C, D](d: Three[R, A, B, C, D]) = d.a1 :: d.a2 :: d.a3 :: HNil
  // def toList[R[_, _], A, B, C, D, E](d: Four[R, A, B, C, D, E]) = d.a1 :: d.a2 :: d.a3 :: d.a4 :: HNil

  // def fromList[R[_, _], A, B, C, D, E](l: R[A, B] :: R[B, C] :: R[C, D] :: R[D, E] :: HNil) = Four(l(0), l(1), l(2), l(3))
  // def fromList[R[_, _], A, B, C, D](l: R[A, B] :: R[B, C] :: R[C, D] :: HNil) = Three(l(0), l(1), l(2))
  // def fromList[R[_, _], A, B, C](l: R[A, B] :: R[B, C] :: HNil) = Two(l(0), l(1))
  // def fromList[R[_, _], A, B](l: R[A, B] :: HNil) = One(l(0))

  // def DigittoTree[R[_, _], A, B](digit: One[R, A, B]): TFingerTree[R, A, B] = single(digit.a1)

  // def DigittoTree[R[_, _], A, B, C](digit: Two[R, A, B, C]): TFingerTree[R, A, C] =
  //   deep[R, One[R, A, B], One[R, B, C], A, B, B, C](One(digit.a1), empty[({ type N[U, V] = Node[R, U, V] })#N, B], One(digit.a2))

  // def DigittoTree[R[_, _], A, B, C, D](digit: Three[R, A, B, C, D]): TFingerTree[R, A, D] =
  //   deep[R, Two[R, A, B, C], One[R, C, D], A, C, C, D](Two(digit.a1, digit.a2), empty[({ type N[U, V] = Node[R, U, V] })#N, C], One(digit.a3))

  // def DigittoTree[R[_, _], A, B, C, D, E](digit: Four[R, A, B, C, D, E]): TFingerTree[R, A, E] =
  //   deep[R, Two[R, A, B, C], Two[R, C, D, E], A, C, C, E](Two(digit.a1, digit.a2), empty[({ type N[U, V] = Node[R, U, V] })#N, C], Two(digit.a3, digit.a4))

  // def appendd[R[_, _], A, B, C](d1: One[R, A, B], d2: One[R, B, C]): Digit[R, A, C] = Two(d1.a1, d2.a1)
  // def appendd[R[_, _], A, B, C, D](d1: One[R, A, B], d2: Two[R, B, C, D]): Digit[R, A, D] = Three(d1.a1, d2.a1, d2.a2)
  // def appendd[R[_, _], A, B, C, D](d1: Two[R, A, B, C], d2: One[R, C, D]): Digit[R, A, D] = Three(d1.a1, d1.a2, d2.a1)
  // def appendd[R[_, _], A, B, C, D, E](d1: One[R, A, B], d2: Three[R, B, C, D, E]): Digit[R, A, E] = Four(d1.a1, d2.a1, d2.a2, d2.a3)
  // def appendd[R[_, _], A, B, C, D, E](d1: Two[R, A, B, C], d2: Two[R, C, D, E]): Digit[R, A, E] = Four(d1.a1, d1.a2, d2.a1, d2.a2)
  // def appendd[R[_, _], A, B, C, D, E](d1: Three[R, A, B, C, D], d2: One[R, D, E]): Digit[R, A, E] = Four(d1.a1, d1.a2, d1.a3, d2.a1)


