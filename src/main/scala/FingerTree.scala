import shapeless._


sealed abstract class FingerTree


object FingerTree {
  implicit class FingerTreeOps[T <: FingerTree](val t: T) extends AnyVal {

    /** Prepend element to fingertree */
    def +:[A, Out <: FingerTree](a: A)(
      implicit p: Prepend.Case.Aux[A, T, Out]
    ): Out = p(a, t)

    /** Append element to fingertree */
    def :+[A, Out <: FingerTree](a: A)(
      implicit p: Append.Case.Aux[T, A, Out]
    ): Out = p(t, a)

    /** Append element to fingertree */
    def ++[T2 <: FingerTree, Out <: FingerTree](t2: T2)(
      implicit p: Add.Case.Aux[T, HNil, T2, Out]
    ): Out = p(t :: HNil :: t2 :: HNil)

  }

  import Node._
  import Digit._

  case object Empty extends FingerTree

  case class Single[A](a: A) extends FingerTree

  case class Deep[
    PR <: Digit,
    MD <: FingerTree,
    SF <: Digit
  ](
    prefix: PR,
    middle: MD,
    suffix: SF
  ) extends FingerTree

  /** Creates Empty FingerTree */
  def empty(): Empty.type = Empty

  /** Creates Single FingerTree */
  def single[A](a: => A): Single[A] = new Single[A](a)

  /** Creates Deep FingerTree */
  def deep[
    PR <: Digit,
    MD <: FingerTree,
    SF <: Digit
  ](
    prefix: PR,
    middle: => MD,
    suffix: SF
  ): Deep[PR, MD, SF] = new Deep[PR, MD, SF](prefix, middle, suffix)

  /** Prepend element to fingertree */
  def prepend[
    A,
    T   <: FingerTree,
    Out <: FingerTree
  ](
    a:    A,
    tree: T
  )(
    implicit p: Prepend.Case.Aux[A, T, Out]
  ): Out = p(a, tree)


  /** Append element to fingertree */
  def append[
    T   <: FingerTree,
    A,
    Out <: FingerTree
  ](
    tree: T,
    a:    A
  )(
    implicit p: Append.Case.Aux[T, A, Out]
  ): Out = p(tree, a)


  /** Adds HList on the left */
  def addAllL[
    HL <: HList,
    T  <: FingerTree,
    Out
  ](
    l:    HL,
    tree: T
  )(implicit addAllL: AddAllL.Case.Aux[HL, T, Out]): Out = addAllL(l, tree)


  /** Adds HList on the right */
  def addAllR[
    T   <: FingerTree,
    HL <: HList
  ](
    tree: T,
    l:    HL
  )(
    implicit addAllR: AddAllR.Case[T, HL]
  ): addAllR.Result = addAllR(tree, l)


  /** Adds FingerTree + HList + FingerTree*/
  def add[
    T1 <: FingerTree,
    HL <: HList,
    T2 <: FingerTree
  ](
    t1: T1,
    l: HL,
    t2: T2
  )(
    implicit add: Add.Case[T1, HL, T2]
  ): add.Result = add(t1 :: l :: t2 :: HNil)


  /** Creates Fingertree by inserting list on left of middle + suffix */
  def deepl[
    HL  <: HList,
    MD  <: FingerTree,
    SF  <: Digit,
    Out <: FingerTree
  ](
    l:    HL,
    tree: MD,
    sf:   SF
  )(
    implicit d: DeepL.Case.Aux[HL, MD, SF, Out]
  ): Out = d(l :: tree :: sf :: HNil)


  object DeepL extends Poly3 {

    implicit def caseHL[
      HL  <: HList,
      MD  <: FingerTree,
      SF  <: Digit,
      Out <: Digit
    ](implicit
      fromList: Digit.FromList.Case.Aux[HL, Out]
    ): Case.Aux[HL, MD, SF, Deep[Out, MD, SF]] = at[HL, MD, SF] { (prefix, m, suffix) =>
      deep[Out, MD, SF](fromList(prefix), m, suffix)
    }


    implicit def caseHNilEmpty[
      SF  <: Digit,
      Out <: FingerTree
    ](implicit
      digitToTree: Digit.ToTree.Case.Aux[SF, Out]
    ): Case.Aux[HNil, Empty.type, SF, Out] = at[HNil, Empty.type, SF] { (prefix, m, suffix) =>
      digitToTree(suffix)
    }


    implicit def caseHNilSingle[
      SF  <: Digit,
      Out <: FingerTree,
      A
    ](implicit
      digitToTree: Digit.ToTree.Case.Aux[SF, Out]
    ): Case.Aux[HNil, Single[A], SF, Deep[One[A], Empty.type, SF]] = at[HNil, Single[A], SF] { (prefix, m, suffix) =>
      deep(One(m.a), Empty, suffix)
    }

    implicit def caseHNilDeep[
      PR  <: Digit,
      MD  <: FingerTree,
      SF  <: Digit,
      SF2 <: Digit,
      MD2 <: FingerTree,
      H   <: Node,
      T   <: HList,
      HD  <: Digit
    ](implicit
      toList: Digit.ToList.Case.Aux[PR, H :: T],
      deepl: DeepL.Case.Aux[T, MD, SF, MD2],
      nodeToDigit: Node.ToDigit.Case.Aux[H, HD]
    ): Case.Aux[HNil, Deep[PR, MD, SF], SF2, Deep[HD, MD2, SF2]] = at[HNil, Deep[PR, MD, SF], SF2] { (prefix, m, suffix) =>
      val l = toList(m.prefix)
      val m2 = deepl(l.tail :: m.middle :: m.suffix :: HNil)
      deep(nodeToDigit(l.head), m2, suffix)
    }
  }

  object Prepend extends Poly2 {
    implicit def caseEmpty[A]: Case.Aux[A, Empty.type, Single[A]] =
      at[A, Empty.type] { (a, t) => single(a) }

    implicit def caseSingle[A, B]: Case.Aux[A, Single[B], Deep[One[A], Empty.type, One[B]]] =
      at[A, Single[B]] { (e, t) =>
        deep(One(e), Empty, One(t.a))
      }

    implicit def caseFour[
      A, B, C, D, E,
      MD  <: FingerTree,
      SF  <: Digit,
      Out <: FingerTree
    ](implicit
      prepend: Case.Aux[Node3[C, D, E], MD, Out]
    ): Case.Aux[A, Deep[Four[B, C, D, E], MD, SF], Deep[Two[A, B], Out, SF]] = 
      at[A, Deep[Four[B, C, D, E], MD, SF]] { (a, t) =>
        deep(
          Two(a, t.prefix.a1),
          prepend(Node3(t.prefix.a2, t.prefix.a3, t.prefix.a4), t.middle),
          t.suffix
        )
      }

    implicit def caseLast[
      A,
      PR  <: Digit,
      MD  <: FingerTree,
      SF  <: Digit,
      Out <: Digit
    ](implicit
      append: Digit.Append.Case.Aux[One[A], PR, Out]
    ): Case.Aux[A, Deep[PR, MD, SF], Deep[Out, MD, SF]] = 
      at[A, Deep[PR, MD, SF]] { (a, t) =>
        deep(
          append(One(a), t.prefix),
          t.middle,
          t.suffix
        )
      }
  }


  object Append extends Poly2 {
    implicit def caseEmpty[A] = at[Empty.type, A] { (t, a) => single(a) }

    implicit def caseSingle[A, B] = at[Single[B], A] { (t, e) =>
      deep(One(t.a), Empty, One(e))
    }

    implicit def caseFour[
      A, B, C, D, E,
      MD  <: FingerTree,
      PR  <: Digit,
      Out <: FingerTree
    ](implicit
      append: Append.Case.Aux[MD, Node3[A, B, C], Out]
    ): Case.Aux[Deep[PR, MD, Four[A, B, C, D]], E, Deep[PR, Out, Two[D, E]]] =
      at[Deep[PR, MD, Four[A, B, C, D]], E] { (t, e) =>
        deep(
          t.prefix,
          append(t.middle, Node3(t.suffix.a1, t.suffix.a2, t.suffix.a3)),
          Two(t.suffix.a4, e)
        )
      }

    implicit def caseLast[
      A,
      PR  <: Digit,
      MD  <: FingerTree,
      SF  <: Digit,
      Out <: Digit
    ](implicit
      append: Digit.Append.Case.Aux[SF, One[A], Out]
    ): Case.Aux[Deep[PR, MD, SF], A, Deep[PR, MD, Out]] = at[Deep[PR, MD, SF], A] { (t, a) =>
      deep(
        t.prefix,
        t.middle,
        append(t.suffix, One(a))
      )
    }
  }

  object AddAllL extends Poly2 {
    implicit def caseHNil[T <: FingerTree]: Case.Aux[HNil, T, T] = at[HNil, T] { (l, t) => t }
    //implicit def caseHNilType[T <: FingerTree]: Case.Aux[HNil.type, T, T] = at[HNil.type, T] { (l, t) => t }


    /** HACK SCALAC WHICH CANT INFER THIS CASE FOR UNKNOWN REASON
      * BUG OR SOMETHING I DONT SEE IN MY CODE????
      */
    implicit def caseHList2[
      H1, H2, H3, H4, T <: FingerTree,
      Out1 <: FingerTree,
      Out2 <: FingerTree,
      Out3 <: FingerTree,
      Out4 <: FingerTree
    ](implicit
      prepend1: Prepend.Case.Aux[H4, T, Out1],
      prepend2: Prepend.Case.Aux[H3, Out1, Out2],
      prepend3: Prepend.Case.Aux[H2, Out2, Out3],
      prepend4: Prepend.Case.Aux[H1, Out3, Out4]
    ): Case.Aux[H1 :: H2 :: H3 :: H4 :: HNil, T, Out4] =
      at[H1 :: H2 :: H3 :: H4 :: HNil, T] { (l, t) =>
        prepend4(l(0), prepend3(l(1), prepend2(l(2), prepend1(l(3), t))))
      }

    implicit def caseHList[
      HH, HT <: HList, T <: FingerTree, Out1 <: FingerTree, Out2 <: FingerTree
    ](implicit
      addAllL: AddAllL.Case.Aux[HT, T, Out1],
      prepend: Prepend.Case.Aux[HH, Out1, Out2]
    ): Case.Aux[HH :: HT, T, Out2] =
      at[HH :: HT, T] { (l, t) =>
        prepend(l.head, addAllL(l.tail, t))
      }
  }

  object AddAllR extends Poly2 {
    implicit def caseHNil[T <: FingerTree]: Case.Aux[T, HNil, T] = at[T, HNil] { (t, l) => t }

    /** HACK SCALAC WHICH CANT INFER THIS CASE FOR UNKNOWN REASON
      * BUG OR SOMETHING I DONT SEE IN MY CODE????
      */
    implicit def caseHList2[
      H1, H2, H3, H4, T <: FingerTree,
      Out1 <: FingerTree,
      Out2 <: FingerTree,
      Out3 <: FingerTree,
      Out4 <: FingerTree
    ](implicit
      append1: Append.Case.Aux[T, H1, Out1],
      append2: Append.Case.Aux[Out1, H2, Out2],
      append3: Append.Case.Aux[Out2, H3, Out3],
      append4: Append.Case.Aux[Out3, H4, Out4]
    ): Case.Aux[T, H1 :: H2 :: H3 :: H4 :: HNil, Out4] =
      at[T, H1 :: H2 :: H3 :: H4 :: HNil] { (t, l) =>
        append4(append3(append2(append1(t, l(0)), l(1)), l(2)), l(3))
      }

    implicit def caseHList[
      HH, HT <: HList, T <: FingerTree, Out1 <: FingerTree, Out2 <: FingerTree
    ](implicit
      append: Append.Case.Aux[T, HH, Out1],
      addAllR: AddAllR.Case.Aux[Out1, HT, Out2]
    ): Case.Aux[T, HH :: HT, Out2] =
      at[T, HH :: HT] { (t, l) =>
        addAllR(append(t, l.head), l.tail)
      }
  }


  object Add extends Poly3 {
    implicit def caseEmptyEmpty[HL <: HList, Out <: FingerTree](
      implicit addAllL: AddAllL.Case.Aux[HL, Empty.type, Out]
    ): Case.Aux[Empty.type, HL, Empty.type, Out] =
      at[Empty.type, HL, Empty.type] { (t1, l, t2) => addAllL(l, Empty) }

    implicit def caseSingleEmpty[
      A, HL <: HList, Out1 <: FingerTree, Out2 <: FingerTree
    ](implicit
        addAllL: AddAllL.Case.Aux[HL, Empty.type, Out1],
        prepend: Prepend.Case.Aux[A, Out1, Out2]
    ): Case.Aux[Single[A], HL, Empty.type, Out2] = at[Single[A], HL, Empty.type] { (t1, l, t2) => prepend(t1.a, addAllL(l, t2)) }

    implicit def caseEmptySingle[
      A, HL <: HList, Out1 <: FingerTree, Out2 <: FingerTree
    ](implicit
        addAllR: AddAllR.Case.Aux[Empty.type, HL, Out1],
        append: Append.Case.Aux[Out1, A, Out2]
    ): Case.Aux[Empty.type, HL, Single[A], Out2] = at[Empty.type, HL, Single[A]] { (t1, l, t2) => append(addAllR(t1, l), t2.a) }

    implicit def caseEmpty1[HL <: HList, T <: FingerTree, Out <: FingerTree](
      implicit addAllL: AddAllL.Case.Aux[HL, T, Out]
    ): Case.Aux[Empty.type, HL, T, Out] =
      at[Empty.type, HL, T] { (t1, l, t2) => addAllL(l, t2) }

    implicit def caseEmpty2[T <: FingerTree, HL <: HList, Out <: FingerTree](
      implicit addAllR: AddAllR.Case.Aux[T, HL, Out]
    ): Case.Aux[T, HL, Empty.type, Out] =
      at[T, HL, Empty.type] { (t1, l, t2) => addAllR(t1, l) }

    implicit def caseSingleSingle[
      A, B, HL <: HList, Out1 <: FingerTree, Out2 <: FingerTree
    ](implicit
        addAllL: AddAllL.Case.Aux[HL, Single[B], Out1],
        prepend: Prepend.Case.Aux[A, Out1, Out2]
    ): Case.Aux[Single[A], HL, Single[B], Out2] = at[Single[A], HL, Single[B]] { (t1, l, t2) => prepend(t1.a, addAllL(l, t2)) }

    implicit def caseSingle1[
      A, T <: FingerTree, HL <: HList, Out1 <: FingerTree, Out2 <: FingerTree
    ](implicit
        addAllL: AddAllL.Case.Aux[HL, T, Out1],
        prepend: Prepend.Case.Aux[A, Out1, Out2]
    ): Case.Aux[Single[A], HL, T, Out2] = at[Single[A], HL, T] { (t1, l, t2) => prepend(t1.a, addAllL(l, t2)) }

    implicit def caseSingle2[
      A, T <: FingerTree, HL <: HList, Out1 <: FingerTree, Out2 <: FingerTree
    ](implicit
        addAllR: AddAllR.Case.Aux[T, HL, Out1],
        append: Append.Case.Aux[Out1, A, Out2]
    ): Case.Aux[T, HL, Single[A], Out2] = at[T, HL, Single[A]] { (t1, l, t2) => append(addAllR(t1, l), t2.a) }

    implicit def caseDeep[
      PR1 <: Digit,      PR2 <: Digit,
      MD1 <: FingerTree, MD2 <: FingerTree,
      SF1 <: Digit,      SF2 <: Digit,
      HL  <: HList, HSF1 <: HList, HPR2 <: HList, HOut1 <: HList, HOut2 <: HList, HOut3 <: HList,
      T3 <: FingerTree
    ](implicit
      toList1: Digit.ToList.Case.Aux[SF1, HSF1],
      toList2: Digit.ToList.Case.Aux[PR2, HPR2],
      hlistPrepend1: shapeless.ops.hlist.Prepend.Aux[HL, HPR2, HOut1],
      hlistPrepend2: shapeless.ops.hlist.Prepend.Aux[HSF1, HOut1, HOut2],
      fromListNodes: Node.FromList.Case.Aux[HOut2, HOut3],
      add: Add.Case.Aux[MD1, HOut3, MD2, T3]
    ): Case.Aux[Deep[PR1, MD1, SF1], HL, Deep[PR2, MD2, SF2], Deep[PR1, T3, SF2]] =
      at[Deep[PR1, MD1, SF1], HL, Deep[PR2, MD2, SF2]] { (t1, l, t2) =>
        deep(
          t1.prefix,
          add(
            t1.middle ::
            fromListNodes(
              hlistPrepend2(
                toList1(t1.suffix),
                hlistPrepend1(l, toList2(t2.prefix))
              )
            ) :: t2.middle :: HNil),
          t2.suffix
        )
      }
  }


  sealed abstract class Node

  object Node {
    case class Node2[A1, A2](a1: A1, a2: A2) extends Node
    case class Node3[A1, A2, A3](a1: A1, a2: A2, a3: A3) extends Node

    object ToDigit extends Poly1 {
      implicit def caseNode2[A1, A2] = at[Node2[A1, A2]] { n => Digit.Two(n.a1, n.a2) }
      implicit def caseNode3[A1, A2, A3] = at[Node3[A1, A2, A3]] { n => Digit.Three(n.a1, n.a2, n.a3) }
    }

    object FromList extends Poly1 {
      implicit def case2[A, B]: Case.Aux[A :: B :: HNil, Node2[A, B] :: HNil] =
        at[A :: B :: HNil] { case a1 :: a2 :: HNil => Node2(a1, a2) :: HNil }

      implicit def case3[A, B, C]: Case.Aux[A :: B :: C :: HNil, Node3[A, B, C] :: HNil] =
        at[A :: B :: C :: HNil] { case a1 :: a2 :: a3 :: HNil => Node3(a1, a2, a3) :: HNil }

      implicit def case4[A, B, C, D]: Case.Aux[A :: B :: C :: D :: HNil, Node2[A, B] :: Node2[C, D] :: HNil] =
        at[A :: B :: C :: D :: HNil] { case a1 :: a2 :: a3 :: a4 :: HNil => Node2(a1, a2) :: Node2(a3, a4) :: HNil }

      implicit def caseOthers[A, B, C, T <: HList, Out <: HList](
        implicit fromList: FromList.Case.Aux[T, Out]
      ): Case.Aux[A :: B :: C :: T, Node3[A, B, C] :: Out] =
        at[A :: B :: C :: T] { case a1 :: a2 :: a3 :: t => Node3(a1, a2, a3) :: fromList(t) }
    }
  }

  sealed abstract class Digit

  object Digit {
    // TODO ADD FCT BY_VAL TO BY_NAME
    case class One[A1](a1: A1) extends Digit
    case class Two[A1, A2](a1: A1, a2: A2) extends Digit
    case class Three[A1, A2, A3](a1: A1, a2: A2, a3: A3) extends Digit
    case class Four[A1, A2, A3, A4](a1: A1, a2: A2, a3: A3, a4: A4) extends Digit

    object ToList extends Poly1 {
      implicit def caseOne[A1] = at[One[A1]] { d => d.a1 :: HNil }
      implicit def caseTwo[A1, A2] = at[Two[A1, A2]] { d => d.a1 :: d.a2 :: HNil }
      implicit def caseThree[A1, A2, A3] = at[Three[A1, A2, A3]] { d => d.a1 :: d.a2 :: d.a3 :: HNil }
      implicit def caseFour[A1, A2, A3, A4] = at[Four[A1, A2, A3, A4]] { d => d.a1 :: d.a2 :: d.a3 :: d.a4 :: HNil }
    }

    object FromList extends Poly1 {
      implicit def case4[A1, A2, A3, A4] = at[A1 :: A2 :: A3 :: A4 :: HNil]( l => Four(l(0), l(1), l(2), l(3)) )
      implicit def case3[A1, A2, A3] = at[A1 :: A2 :: A3 :: HNil]( l => Three(l(0), l(1), l(2)) )
      implicit def case2[A1, A2] = at[A1 :: A2 :: HNil]( l => Two(l(0), l(1)) )
      implicit def case1[A1] = at[A1 :: HNil]( l => One(l(0)) )
    }


    object Append extends Poly2 {

      implicit def caseOneOne[A1, A2]: Case.Aux[One[A1], One[A2], Two[A1, A2]] =
        at[One[A1], One[A2]] { (d1, d2) => Two(d1.a1, d2.a1) }

      implicit def caseOneTwo[A1, A2, A3]: Case.Aux[One[A1], Two[A2, A3], Three[A1, A2, A3]] =
        at[One[A1], Two[A2, A3]] { (d1, d2) => Three(d1.a1, d2.a1, d2.a2) }

      implicit def caseTwoOne[A1, A2, A3]: Case.Aux[Two[A1, A2], One[A3], Three[A1, A2, A3]] =
        at[Two[A1, A2], One[A3]] { (d1, d2) => Three(d1.a1, d1.a2, d2.a1) }

      implicit def caseOneThree[A1, A2, A3, A4]: Case.Aux[One[A1], Three[A2, A3, A4], Four[A1, A2, A3, A4]] =
        at[One[A1], Three[A2, A3, A4]] { (d1, d2) => Four(d1.a1, d2.a1, d2.a2, d2.a3) }

      implicit def caseTwoTwo[A1, A2, A3, A4]: Case.Aux[Two[A1, A2], Two[A3, A4], Four[A1, A2, A3, A4]] =
        at[Two[A1, A2], Two[A3, A4]] { (d1, d2) => Four(d1.a1, d1.a2, d2.a1, d2.a2) }

      implicit def caseThreeOne[A1, A2, A3, A4]: Case.Aux[Three[A1, A2, A3], One[A4], Four[A1, A2, A3, A4]] =
        at[Three[A1, A2, A3], One[A4]] { (d1, d2) => Four(d1.a1, d1.a2, d1.a3, d2.a1) }

    }

    object ToTree extends Poly1 {
      implicit def caseOne[A]: Case.Aux[One[A], Single[A]] = at[One[A]] { d => single(d.a1) }

      implicit def caseTwo[A1, A2]: Case.Aux[Two[A1, A2], Deep[One[A1], Empty.type, One[A2]]] = at[Two[A1, A2]] { d =>
        deep[
          One[A1],
          Empty.type,
          One[A2]
        ](One(d.a1), Empty, One(d.a2))
      }

      implicit def caseThree[A1, A2, A3]: Case.Aux[Three[A1, A2, A3], Deep[Two[A1, A2], Empty.type, One[A3]]] = at[Three[A1, A2, A3]] { d =>
        deep[
          Two[A1, A2],
          Empty.type,
          One[A3]
        ](Two(d.a1, d.a2), Empty, One(d.a3))
      }

      implicit def caseFour[A1, A2, A3, A4]: Case.Aux[Four[A1, A2, A3, A4], Deep[Two[A1, A2], Empty.type, Two[A3, A4]]] = at[Four[A1, A2, A3, A4]] { d =>
        deep[
          Two[A1, A2],
          Empty.type,
          Two[A3, A4]
        ](Two(d.a1, d.a2), Empty, Two(d.a3, d.a4))
      }
    }
  }

}

