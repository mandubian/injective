import shapeless._


sealed abstract class FingerTree

object FingerTree {

  sealed abstract class Node

  object Node {
    case class Node2[A1, A2](a1: A1, a2: A2) extends Node
    case class Node3[A1, A2, A3](a1: A1, a2: A2, a3: A3) extends Node

    object ToDigit extends Poly1 {
      implicit def caseNode2[A1, A2] = at[Node2[A1, A2]] { n => Digit.Two(n.a1, n.a2) }
      implicit def caseNode3[A1, A2, A3] = at[Node3[A1, A2, A3]] { n => Digit.Three(n.a1, n.a2, n.a3) }
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

      implicit def caseOneOne[A1, A2] =
        at[One[A1], One[A2]] { (d1, d2) => Two(d1.a1, d2.a1) }

      implicit def caseOneTwo[A1, A2, A3] =
        at[One[A1], Two[A2, A3]] { (d1, d2) => Three(d1.a1, d2.a1, d2.a2) }

      implicit def caseTwoOne[A1, A2, A3] =
        at[Two[A1, A2], One[A3]] { (d1, d2) => Three(d1.a1, d1.a2, d2.a1) }

      implicit def caseOneThree[A1, A2, A3, A4] =
        at[One[A1], Three[A2, A3, A4]] { (d1, d2) => Four(d1.a1, d2.a1, d2.a2, d2.a3) }

      implicit def caseTwoTwo[A1, A2, A3, A4] =
        at[Two[A1, A2], Two[A3, A4]] { (d1, d2) => Four(d1.a1, d1.a2, d2.a1, d2.a2) }

      implicit def caseThreeOne[A1, A2, A3, A4] =
        at[Three[A1, A2, A4], One[A4]] { (d1, d2) => Four(d1.a1, d1.a2, d1.a3, d2.a1) }

    }
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

  def empty() = Empty

  def single[A](a: => A) = new Single[A](a)

  def deep[
    PR <: Digit,
    MD <: FingerTree,
    SF <: Digit
  ](
    prefix: PR,
    middle: => MD,
    suffix: SF
  ) = new Deep[PR, MD, SF](prefix, middle, suffix)


  object DigitToTree extends Poly1 {
    implicit def caseOne[A] = at[One[A]] { d => single(d.a1) }

    implicit def caseTwo[A1, A2] = at[Two[A1, A2]] { d =>
      deep[
        One[A1],
        Empty.type,
        One[A2]
      ](One(d.a1), Empty, One(d.a2))
    }

    implicit def caseThree[A1, A2, A3] = at[Three[A1, A2, A3]] { d =>
      deep[
        Two[A1, A2],
        Empty.type,
        One[A3]
      ](Two(d.a1, d.a2), Empty, One(d.a3))
    }

    implicit def caseFour[A1, A2, A3, A4] = at[Four[A1, A2, A3, A4]] { d =>
      deep[
        Two[A1, A2],
        Empty.type,
        Two[A3, A4]
      ](Two(d.a1, d.a2), Empty, Two(d.a3, d.a4))
    }
  }

  def deepl[HL <: HList, MD <: FingerTree, SF <: Digit, Out <: FingerTree](l: HL, tree: MD, sf: SF)(
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
    ) = at[
      HL,
      MD,
      SF
    ] { (prefix, m, suffix) => deep[Out, MD, SF](fromList(prefix), m, suffix) }


    implicit def caseHNilEmpty[
      SF  <: Digit,
      Out <: FingerTree
    ](implicit
      digitToTree: DigitToTree.Case.Aux[SF, Out]
    ) = at[
      HNil,
      Empty.type,
      SF
    ] { (prefix, m, suffix) =>
      digitToTree(suffix)
    }


    implicit def caseHNilSingle[
      SF  <: Digit,
      Out <: FingerTree,
      A
    ](implicit
      digitToTree: DigitToTree.Case.Aux[SF, Out]
    ) = at[
      HNil,
      Single[A],
      SF
    ] { (prefix, m, suffix) =>
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
    ) = at[
      HNil,
      Deep[PR, MD, SF],
      SF2
    ] { (prefix, m, suffix) =>
      val l = toList(m.prefix)
      val m2 = deepl(l.tail :: m.middle :: m.suffix :: HNil)
      deep(nodeToDigit(l.head), m2, suffix)
    }
  }

  def prepend[A, T <: FingerTree, Out <: FingerTree](a: A, tree: T)(implicit p: Prepend.Case.Aux[A, T, Out]): Out = p(a, tree)

  object Prepend extends Poly2 {
    implicit def caseEmpty[A] = at[A, Empty.type] { (a, t) => single(a) }

    implicit def caseSingle[A, B] = at[A, Single[B]] { (e, t) =>
      deep(One(e), Empty, One(t.a))
    }

    implicit def caseFour[
      A, B, C, D,
      MD  <: FingerTree,
      SF  <: Digit,
      Out <: FingerTree
    ](implicit
      prepend: Prepend.Case.Aux[Node3[B, C, D], MD, Out]
    ) = at[
      A,
      Deep[Four[A, B, C, D], MD, SF]
    ] { (a, t) =>
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
    ) = at[
      A,
      Deep[PR, MD, SF]
    ] { (a, t) =>
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
      A, B, C, D,
      MD  <: FingerTree,
      PR  <: Digit,
      Out <: FingerTree
    ](implicit
      append: Append.Case.Aux[MD, Node3[A, B, C], Out]
    ) = at[
      Deep[PR, MD, Four[A, B, C, D]],
      A
    ] { (t, a) =>
      deep(
        t.prefix,
        append(t.middle, Node3(t.suffix.a1, t.suffix.a2, t.suffix.a3)),
        Two(t.suffix.a4, a)
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
    ) = at[
      Deep[PR, MD, SF],
      A
    ] { (t, a) =>
      deep(
        t.prefix,
        t.middle,
        append(t.suffix, One(a))
      )
    }
  }

  object AddAllL extends Poly2 {
    implicit def caseHNil[T <: FingerTree] = at[HNil, T] { (l, t) => t }

    implicit def caseHList[
      HH, HT <: HList, T <: FingerTree, Out1 <: FingerTree, Out2 <: FingerTree
    ](implicit
      addAllL: AddAllL.Case.Aux[HT, T, Out1],
      prepend: Prepend.Case.Aux[HH, Out1, Out2]
    ) = at[HH :: HT, T] { (l, t) =>
      prepend(l.head, addAllL(l.tail, t))
    }
  }

  object AddAllR extends Poly2 {
    implicit def caseHNil[T <: FingerTree] = at[T, HNil] { (t, l) => t }

    implicit def caseHList[
      HH, HT <: HList, T <: FingerTree, Out1 <: FingerTree, Out2 <: FingerTree
    ](implicit
      append: Append.Case.Aux[T, HH, Out1],
      addAllR: AddAllR.Case.Aux[Out1, HT, Out2]
    ) = at[T, HH :: HT] { (t, l) =>
      addAllR(append(t, l.head), l.tail)
    }
  }

  object Add extends Poly3 {
    implicit def caseHNil1[HL <: HList, T <: FingerTree, Out <: FingerTree](
      implicit addAllL: AddAllL.Case.Aux[HL, T, Out]
    ) = at[Empty.type, HL, T] { (t1, l, t2) => addAllL(l, t2) }

    implicit def caseHNil2[T <: FingerTree, HL <: HList, Out <: FingerTree](
      implicit addAllR: AddAllR.Case.Aux[T, HL, Out]
    ) = at[T, HL, Empty.type] { (t1, l, t2) => addAllR(t1, l) }

    implicit def caseSingle1[
      A, T <: FingerTree, HL <: HList, Out1 <: FingerTree, Out2 <: FingerTree
    ](implicit
        addAllL: AddAllL.Case.Aux[HL, T, Out1],
        prepend: Prepend.Case.Aux[A, Out1, Out2]
    ) = at[Single[A], HL, T] { (t1, l, t2) => prepend(t1.a, addAllL(l, t2)) }

    implicit def caseSingle2[
      A, T <: FingerTree, HL <: HList, Out1 <: FingerTree, Out2 <: FingerTree
    ](implicit
        addAllR: AddAllR.Case.Aux[T, HL, Out1],
        append: Append.Case.Aux[Out1, A, Out2]
    ) = at[T, HL, Single[A]] { (t1, l, t2) => append(addAllR(t1, l), t2.a) }

    // implicit def caseDeep[
    //   T1  <: Deep[PR1, MD1, SF1],
    //   T2  <: Deep[PR2, MD2, SF2],
    //   PR1 <: Digit,      PR2 <: Digit,
    //   MD1 <: FingerTree, MD2 <: FingerTree,
    //   SF1 <: Digit,      SF2 <: Digit
    // ](
    //   implicit add: Add.Case.Aux[]
    // ) = at[T1, T2] { (t1, t2) =>
    //   deep(
    //     t1.prefix,

    //     t2.suffix
    //   )
    // }
  }
}
