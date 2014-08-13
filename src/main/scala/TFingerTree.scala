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
}

sealed abstract class TFingerTree[R[_, _], A, B]

object TFingerTree {
  import Digit._
  import TViewl._

  case class Empty[R[_, _], A]() extends TFingerTree[R, A, A]

  case class Single[R[_, _], A, B](a: R[A, B]) extends TFingerTree[R, A, B]

  case class Deep[
    R[_, _],
    PR <: Digit[R, A, B],
    MD <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    SF <: Digit[R, C, D],
    A, B, C, D
  ](
    prefix: PR,
    middle: MD,
    suffix: SF
  ) extends TFingerTree[R, A, D]

  def empty[R[_, _], A] = new Empty[R, A]()

  def single[R[_, _], A, B](a: => R[A, B]) = new Single[R, A, B](a)

  def deep[
    R[_, _],
    PR <: Digit[R, A, B],
    MD <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    SF <: Digit[R, C, D],
    A, B, C, D
  ](
    prefix: PR,
    middle: => MD,
    suffix: SF
  ) = new Deep[R, PR, MD, SF, A, B, C, D](prefix, middle, suffix)


  object ToTree extends Poly1 {
    implicit def caseOne[R[_, _], A, B, C] = at[One[R, A, B]] { d => single(d.a1) }

    implicit def caseTwo[R[_, _], A, B, C] = at[Two[R, A, B, C]] { d =>
      deep[
        R,
        One[R, A, B],
        Empty[({ type N[U, V] = Node[R, U, V] })#N, B],
        One[R, B, C],
        A, B, B, C
      ](One(d.a1), empty[({ type N[U, V] = Node[R, U, V] })#N, B], One(d.a2))
    }

    implicit def caseThree[R[_, _], A, B, C, D] = at[Three[R, A, B, C, D]] { d =>
      deep[
        R,
        Two[R, A, B, C],
        Empty[({ type N[U, V] = Node[R, U, V] })#N, C],
        One[R, C, D],
        A, C, C, D
      ](Two(d.a1, d.a2), empty[({ type N[U, V] = Node[R, U, V] })#N, C], One(d.a3))
    }

    implicit def caseFour[R[_, _], A, B, C, D, E] = at[Four[R, A, B, C, D, E]] { d =>
      deep[
        R,
        Two[R, A, B, C],
        Empty[({ type N[U, V] = Node[R, U, V] })#N, C],
        Two[R, C, D, E],
        A, C, C, E
      ](Two(d.a1, d.a2), empty[({ type N[U, V] = Node[R, U, V] })#N, C], Two(d.a3, d.a4))
    }
  }
  // def toTree[R[_, _], A, B](digit: One[R, A, B]): TFingerTree[R, A, B] = single(digit.a1)

  // def toTree[R[_, _], A, B, C](digit: Two[R, A, B, C]): TFingerTree[R, A, C] =
  //   deep[R, One[R, A, B], One[R, B, C], A, B, B, C](One(digit.a1), empty[({ type N[U, V] = Node[R, U, V] })#N, B], One(digit.a2))

  // def toTree[R[_, _], A, B, C, D](digit: Three[R, A, B, C, D]): TFingerTree[R, A, D] =
  //   deep[R, Two[R, A, B, C], One[R, C, D], A, C, C, D](Two(digit.a1, digit.a2), empty[({ type N[U, V] = Node[R, U, V] })#N, C], One(digit.a3))

  // def toTree[R[_, _], A, B, C, D, E](digit: Four[R, A, B, C, D, E]): TFingerTree[R, A, E] =
  //   deep[R, Two[R, A, B, C], Two[R, C, D, E], A, C, C, E](Two(digit.a1, digit.a2), empty[({ type N[U, V] = Node[R, U, V] })#N, C], Two(digit.a3, digit.a4))

  def appendd[R[_, _], A, B, C](d1: One[R, A, B], d2: One[R, B, C]): Digit[R, A, C] = Two(d1.a1, d2.a1)
  def appendd[R[_, _], A, B, C, D](d1: One[R, A, B], d2: Two[R, B, C, D]): Digit[R, A, D] = Three(d1.a1, d2.a1, d2.a2)
  def appendd[R[_, _], A, B, C, D](d1: Two[R, A, B, C], d2: One[R, C, D]): Digit[R, A, D] = Three(d1.a1, d1.a2, d2.a1)
  def appendd[R[_, _], A, B, C, D, E](d1: One[R, A, B], d2: Three[R, B, C, D, E]): Digit[R, A, E] = Four(d1.a1, d2.a1, d2.a2, d2.a3)
  def appendd[R[_, _], A, B, C, D, E](d1: Two[R, A, B, C], d2: Two[R, C, D, E]): Digit[R, A, E] = Four(d1.a1, d1.a2, d2.a1, d2.a2)
  def appendd[R[_, _], A, B, C, D, E](d1: Three[R, A, B, C, D], d2: One[R, D, E]): Digit[R, A, E] = Four(d1.a1, d1.a2, d1.a3, d2.a1)

  object AppendPoly extends Poly2 {
    implicit def caseOneOne[R[_, _], A, B, C] =
      at[One[R, A, B], One[R, B, C]] { (d1, d2) => Two(d1.a1, d2.a1) }

    implicit def caseOneTwo[R[_, _], A, B, C, D] =
      at[One[R, A, B], Two[R, B, C, D]] { (d1, d2) => Three(d1.a1, d2.a1, d2.a2) }

    implicit def caseTwoOne[R[_, _], A, B, C, D] =
      at[Two[R, A, B, C], One[R, C, D]] { (d1, d2) => Three(d1.a1, d1.a2, d2.a1) }

    implicit def caseOneThree[R[_, _], A, B, C, D, E] =
      at[One[R, A, B], Three[R, B, C, D, E]] { (d1, d2) => Four(d1.a1, d2.a1, d2.a2, d2.a3) }

    implicit def caseTwoTwo[R[_, _], A, B, C, D, E] =
      at[Two[R, A, B, C], Two[R, C, D, E]] { (d1, d2) => Four(d1.a1, d1.a2, d2.a1, d2.a2) }

    implicit def caseThreeOne[R[_, _], A, B, C, D, E] =
      at[Three[R, A, B, C, D], One[R, D, E]] { (d1, d2) => Four(d1.a1, d1.a2, d1.a3, d2.a1) }

  }

  object TViewlPoly extends Poly1 {

    implicit def caseEmpty[R[_, _], A] = at[Empty[R, A]] { e => TEmptyL[TFingerTree, R, A]() }

    implicit def caseSingle[R[_, _], A, B] =
      at[Single[R, A, B]] { s => TLeafL[TFingerTree, R, A, B, B](s.a, Empty[R, B]()) }

    implicit def caseDeep[
      R[_, _],
      PR <: Digit[R, A, B],
      MD <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
      SF <: Digit[R, C, D],
      HT <: HList,
      Out <: Digit[R, X, B],
      A, B, C, D, X
    ](implicit
      c: ToListPoly.Case.Aux[PR, R[A, X] :: HT],
      fromList: FromListPoly.Case.Aux[HT, Out]
    ) = at[Deep[R, PR, MD, SF, A, B, C, D]] { d =>
      ToListPoly(d.prefix) match {
        case h :: t => TLeafL[TFingerTree, R, A, X, D](
          h,
          deepl[R, HT, Out, MD, SF, X, B, C, D](t, d.middle, d.suffix)
        )
      }
    }
  }

  //def tappend[C[_, _], X, Y, Z](a: TFingerTree[C, X, Y], b: TFingerTree[C, Y, Z]): TFingerTree[C, X, Z]

  object ToListPoly extends Poly1 {
    implicit def caseOne[R[_, _], A, B, C] = at[One[R, A, B]] { d => d.a1 :: HNil }
    implicit def caseTwo[R[_, _], A, B, C] = at[Two[R, A, B, C]] { d => d.a1 :: d.a2 :: HNil }
    implicit def caseThree[R[_, _], A, B, C, D] = at[Three[R, A, B, C, D]] { d => d.a1 :: d.a2 :: d.a3 :: HNil }
    implicit def caseFour[R[_, _], A, B, C, D, E] = at[Four[R, A, B, C, D, E]] { d => d.a1 :: d.a2 :: d.a3 :: d.a4 :: HNil }
  }

  object FromListPoly extends Poly1 {
    implicit def case4[R[_, _], A, B, C, D, E] = at[R[A, B] :: R[B, C] :: R[C, D] :: R[D, E] :: HNil]( l => Four(l(0), l(1), l(2), l(3)) )
    implicit def case3[R[_, _], A, B, C, D] = at[R[A, B] :: R[B, C] :: R[C, D] :: HNil]( l => Three(l(0), l(1), l(2)) )
    implicit def case2[R[_, _], A, B, C] = at[R[A, B] :: R[B, C] :: HNil]( l => Two(l(0), l(1)) )
    implicit def case1[R[_, _], A, B] = at[R[A, B] :: HNil]( l => One(l(0)) )
  }

  // def toList[R[_, _], A, B](d: One[R, A, B]) = d.a1 :: HNil
  // def toList[R[_, _], A, B, C](d: Two[R, A, B, C]) = d.a1 :: d.a2 :: HNil
  // def toList[R[_, _], A, B, C, D](d: Three[R, A, B, C, D]) = d.a1 :: d.a2 :: d.a3 :: HNil
  // def toList[R[_, _], A, B, C, D, E](d: Four[R, A, B, C, D, E]) = d.a1 :: d.a2 :: d.a3 :: d.a4 :: HNil

  // def fromList[R[_, _], A, B, C, D, E](l: R[A, B] :: R[B, C] :: R[C, D] :: R[D, E] :: HNil) = Four(l(0), l(1), l(2), l(3))
  // def fromList[R[_, _], A, B, C, D](l: R[A, B] :: R[B, C] :: R[C, D] :: HNil) = Three(l(0), l(1), l(2))
  // def fromList[R[_, _], A, B, C](l: R[A, B] :: R[B, C] :: HNil) = Two(l(0), l(1))
  // def fromList[R[_, _], A, B](l: R[A, B] :: HNil) = One(l(0))

  object ViewToTree extends Poly3 {

    implicit def caseEmpty[
      R[_, _],
      SF   <: Digit[R, C, D],
      MD   <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
      OutSF <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, A, D],
      A, B, C, D
    ](implicit toTree: ToTree.Case.Aux[SF, OutSF]) =
      at[TEmptyL[TFingerTree, ({ type N[U, V] = Node[R, U, V] })#N, A], MD, SF] { (tv, m, sf) => toTree(sf) }

    implicit def caseLeaf[
      R[_, _],
      SF   <: Digit[R, C, D],
      MD   <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
      OutN <: Digit[R, B, X],
      A, B, C, D, X
    ](implicit
      nodeToDigit: NodeToDigit.Case.Aux[Node[R, B, X], OutN]
    ) = at[TLeafL[TFingerTree, ({ type N[U, V] = Node[R, U, V] })#N, B, X, C], MD, SF] { (tv, m, sf) =>
      deep[R, OutN, TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, X, C], SF, B, X, C, D](
        nodeToDigit(tv.head), tv.tail, sf
      )
    }
  }

  object NodeToDigit extends Poly1 {
    import Node._

    implicit def caseNode2[R[_, _], A, B, C] = at[Node2[R, A, B, C]] { n => Two(n.a1, n.a2) }
    implicit def caseNode3[R[_, _], A, B, C, D] = at[Node3[R, A, B, C, D]] { n => Three(n.a1, n.a2, n.a3) }
  }

  def deepl[
    R[_, _],
    HL <: HList,
    Out <: Digit[R, A, B],
    MD <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    SF <: Digit[R, C, D],
    A, B, C, D
  ](
    prefix: HL,
    m: MD,
    suffix: SF
  )(implicit
    fromList: FromListPoly.Case.Aux[HL, Out]
  ): TFingerTree[R, A, D] = deep[R, Out, MD, SF, A, B, C, D](fromList(prefix), m, suffix)

  def deepl[
    R[_, _],
    MD    <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    SF    <: Digit[R, C, D],
    Out   <: TViewl[TFingerTree, ({ type N[U, V] = Node[R, U, V] })#N, B, C],
    OutV  <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, A, D],
    OutSF <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, A, D],
    A, B, C, D
  ](
    prefix: HNil,
    m: MD,
    suffix: SF
  )(implicit
    tviewl: TViewlPoly.Case.Aux[MD, Out],
    viewToTree: ViewToTree.Case.Aux[Out, MD, SF, OutV],
    toTree: ToTree.Case.Aux[SF, OutSF]
  ): TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, A, D] = {
    viewToTree(tviewl(m) :: m :: suffix :: HNil)
  }

  implicit object TFingerTreeTSequence extends TSequence[TFingerTree] {
    def tempty[C[_, _], X]: TFingerTree[C, X, X] = empty[C, X]
    def tsingleton[C[_, _], X, Y](c: C[X, Y]): TFingerTree[C, X, Y] = single[C, X, Y](c)
  }
}



