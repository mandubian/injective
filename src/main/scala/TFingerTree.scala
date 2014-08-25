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

  def toList[R[_, _], A, B](d: Digit[R, A, B]) = d match {
    case One(a1)              => a1 :: HNil
    case Two(a1, a2)          => a1 :: a2 :: HNil
    case Three(a1, a2, a3)    => a1 :: a2 :: a3 :: HNil
    case Four(a1, a2, a3, a4) => a1 :: a2 :: a3 :: a4 :: HNil
  }

  // def fromList[R[_, _], A, B, C, D, E](l: R[A, B] :: R[B, C] :: R[C, D] :: R[D, E] :: HNil) = Four(l(0), l(1), l(2), l(3))
  // def fromList[R[_, _], A, B, C, D](l: R[A, B] :: R[B, C] :: R[C, D] :: HNil) = Three(l(0), l(1), l(2))
  // def fromList[R[_, _], A, B, C](l: R[A, B] :: R[B, C] :: HNil) = Two(l(0), l(1))
  // def fromList[R[_, _], A, B](l: R[A, B] :: HNil) = One(l(0))
}

sealed abstract class TFingerTree[R[_, _], A, B]

object TFingerTree {
  import Digit._
  import TViewl._

  case class Empty[R[_, _], A]() extends TFingerTree[R, A, A]

  case class Single[R[_, _], A, B](a: R[A, B]) extends TFingerTree[R, A, B]

  case class Deep[R[_, _], A, B, C, D](
    prefix: Digit[R, A, B],
    middle: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    suffix: Digit[R, C, D]
  ) extends TFingerTree[R, A, D]

  def empty[R[_, _], A] = new Empty[R, A]()

  def single[R[_, _], A, B](a: => R[A, B]) = new Single[R, A, B](a)

  def deep[R[_, _], A, B, C, D](
    prefix: Digit[R, A, B],
    middle: => TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    suffix: Digit[R, C, D]
  ) = new Deep[R, A, B, C, D](prefix, middle, suffix)


  def prepend[R[_, _], A, B, C, D](a: R[A, B], tree: TFingerTree[R, C, D]): TFingerTree[R, A, D] = {
    tree match {
      case t: Empty[R, A] => Single(a) 
    }
  }

  /*case class Deep[
    PR <: Digit[R, A, B],
    MD <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    SF <: Digit[R, C, D],
    R[_, _], A, B, C, D
  ](
    prefix: PR,
    middle: MD,
    suffix: SF
  ) extends TFingerTree[R, A, D]*/

  /*def deep[
    PR <: Digit[R, A, B],
    MD <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    SF <: Digit[R, C, D],
    R[_, _], A, B, C, D
  ](
    prefix: PR,
    middle: => MD,
    suffix: SF
  ) = new Deep[PR, MD, SF, R, A, B, C, D](prefix, middle, suffix)
  */

  /*object DigitToTree extends Poly1 {
    implicit def caseOne[R[_, _], A, B, C] = at[One[R, A, B]] { d => single(d.a1) }

    implicit def caseTwo[R[_, _], A, B, C] = at[Two[R, A, B, C]] { d =>
      deep[
        One[R, A, B],
        Empty[({ type N[U, V] = Node[R, U, V] })#N, B],
        One[R, B, C],
        R, A, B, B, C
      ](One(d.a1), empty[({ type N[U, V] = Node[R, U, V] })#N, B], One(d.a2))
    }

    implicit def caseThree[R[_, _], A, B, C, D] = at[Three[R, A, B, C, D]] { d =>
      deep[
        Two[R, A, B, C],
        Empty[({ type N[U, V] = Node[R, U, V] })#N, C],
        One[R, C, D],
        R, A, C, C, D
      ](Two(d.a1, d.a2), empty[({ type N[U, V] = Node[R, U, V] })#N, C], One(d.a3))
    }

    implicit def caseFour[R[_, _], A, B, C, D, E] = at[Four[R, A, B, C, D, E]] { d =>
      deep[
        Two[R, A, B, C],
        Empty[({ type N[U, V] = Node[R, U, V] })#N, C],
        Two[R, C, D, E],
        R, A, C, C, E
      ](Two(d.a1, d.a2), empty[({ type N[U, V] = Node[R, U, V] })#N, C], Two(d.a3, d.a4))
    }
  }

  object AppendDigitPoly extends Poly2 {
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

  }*/

  /*object PrependEltPoly extends Poly2 {
    implicit def caseEmpty[R[_, _], A, B] = at[R[A, B], Empty[R, B]] { (e, t) => single(e) }

    implicit def caseSingle[R[_, _], A, B, C] = at[R[A, B], Single[R, B, C]] { (e, t) =>
      deep[
        One[R, A, B],
        Empty[({ type N[U, V] = Node[R, U, V] })#N, B],
        One[R, B, C],
        R, A, B, B, C
      ](One(e), empty[({ type N[U, V] = Node[R, U, V] })#N, B], One(t.a))
    }

    implicit def caseDeepFour[
      R[_, _], A, B, C, D, E, F, G, H,
      PR  <: Four[R, B, C, D, E, F],
      MD  <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, F, G],
      SF  <: Digit[R, G, H],
      Out <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, C, G]
    ](implicit
      c: PrependEltPoly.Case.Aux[Node.Node3[R, C, D, E, F], MD, Out]
    ) = at[R[A, B], Deep[PR, MD, SF, R, B, C, F, H]] { (e, t) =>
      deep[
        Two[R, A, B, C],
        Out,
        One[R, G, H],
        R, A, C, G, H
      ](Two(e, t.prefix.a1),
        c(
          Node.Node3[R, C, D, E, F](t.prefix.a2, t.prefix.a3, t.prefix.a4),
          t.middle
        ),
        t.suffix
      )
    }
  }*/

  /*object AppendTreePoly extends Poly2 {
    implicit def caseEmpty_Tree[
      R[_, _], A, B,
      FT <: TFingerTree[R, A, B]
    ] = at[Empty[R, A], FT] { (e, t) => t }

    implicit def caseTree_Empty[
      R[_, _], A, B,
      FT <: TFingerTree[R, A, B]
    ] = at[FT, Empty[R, B]] { (e, t) => t }

    // implicit def caseSingle_Tree[
    //   R[_, _], A, B, C
    //   FT <: TFingerTree[R, B, C]
    // ] = at[Single[R, A, B], FT] { (e, t) => t }
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
    ) = at[Deep[PR, MD, SF, R, A, B, C, D]] { d =>
      ToListPoly(d.prefix) match {
        case h :: t => TLeafL[TFingerTree, R, A, X, D](
          h,
          deepl[HT, Out, MD, SF, R, X, B, C, D](t, d.middle, d.suffix)
        )
      }
    }
  }

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

  object ViewDigitToTree extends Poly3 {

    implicit def caseEmpty[
      R[_, _],
      SF   <: Digit[R, C, D],
      MD   <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
      OutSF <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, A, D],
      A, B, C, D
    ](implicit DigittoTree: DigitToTree.Case.Aux[SF, OutSF]) =
      at[TEmptyL[TFingerTree, ({ type N[U, V] = Node[R, U, V] })#N, A], MD, SF] { (tv, m, sf) => DigittoTree(sf) }

    implicit def caseLeaf[
      R[_, _],
      SF   <: Digit[R, C, D],
      MD   <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
      OutN <: Digit[R, B, X],
      A, B, C, D, X
    ](implicit
      nodeToDigit: NodeToDigit.Case.Aux[Node[R, B, X], OutN]
    ) = at[TLeafL[TFingerTree, ({ type N[U, V] = Node[R, U, V] })#N, B, X, C], MD, SF] { (tv, m, sf) =>
      deep[OutN, TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, X, C], SF, R, B, X, C, D](
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
    HL <: HList,
    Out <: Digit[R, A, B],
    MD <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    SF <: Digit[R, C, D],
    R[_, _], A, B, C, D
  ](
    prefix: HL,
    m: MD,
    suffix: SF
  )(implicit
    fromList: FromListPoly.Case.Aux[HL, Out]
  ): TFingerTree[R, A, D] = deep[Out, MD, SF, R, A, B, C, D](fromList(prefix), m, suffix)

  def deepl[
    MD    <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, B, C],
    SF    <: Digit[R, C, D],
    Out   <: TViewl[TFingerTree, ({ type N[U, V] = Node[R, U, V] })#N, B, C],
    OutV  <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, A, D],
    OutSF <: TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, A, D],
    R[_, _], A, B, C, D
  ](
    prefix: HNil,
    m: MD,
    suffix: SF
  )(implicit
    tviewl: TViewlPoly.Case.Aux[MD, Out],
    viewDigitToTree: ViewDigitToTree.Case.Aux[Out, MD, SF, OutV],
    DigittoTree: DigitToTree.Case.Aux[SF, OutSF]
  ): TFingerTree[({ type N[U, V] = Node[R, U, V] })#N, A, D] = {
    viewDigitToTree(tviewl(m) :: m :: suffix :: HNil)
  }

  implicit object TFingerTreeTSequence extends TSequence[TFingerTree] {
    def tempty[C[_, _], X]: TFingerTree[C, X, X] = empty[C, X]
    def tsingleton[C[_, _], X, Y](c: C[X, Y]): TFingerTree[C, X, Y] = single[C, X, Y](c)
  }*/
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


