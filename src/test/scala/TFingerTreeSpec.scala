/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
import org.scalatest._

import scalaz.{Free, Coyoneda}

import scala.concurrent._
import shapeless.test._
import scala.collection.{ GenTraversable, GenTraversableLike }
import scala.collection.generic.CanBuildFrom

import nl.grons.metrics.scala._


class TFingerTreeSpec extends FlatSpec with Matchers{
  import shapeless._
  import poly._
  import TFingerTree._
  import Digit._

  "TFingerTree" should "prepend" in {

    type R[A, B] = A => Id[B]

    val t = TFingerTree.empty[R, String]()
    println("Tree:"+t)

    val f = (i:Int) => i.toString

    val t2 = prepend(f, t)
    t2 should equal (Single(f))
    println("T2: " + t2)

    val f2 = (d:Float) => d.toInt

    val t3 = prepend(f2, t2)
    t3 should equal (Deep(One(f2), Empty[({ type N[U, V] = Node[R, U, V] })#N, Int](), One(f)))
    println("T3: " + t3)

    val f3 = (d:Double) => d.toFloat

    val t4 = prepend(f3, t3)
    t4 should equal (Deep(Two(f3, f2), Empty[({ type N[U, V] = Node[R, U, V] })#N, Int](), One(f)))
    println("T4: " + t4)

  }

  "TFingerTree" should "append" in {
    type R[A, B] = A => Id[B]

    val t = TFingerTree.empty[R, Double]()
    println("Tree:"+t)

    val f = (i:Double) => i.toFloat

    val t2 = append(t, f)
    t2 should equal (Single(f))
    println("T2: " + t2)

    val f2 = (i:Float) => i.toInt

    val t3 = append(t2, f2)
    t3 should equal (Deep(One(f), Empty[({ type N[U, V] = Node[R, U, V] })#N, Float](), One(f2)))
    println("T3: " + t3)

    val f3 = (i:Int) => i.toString

    val t4 = append(t3, f3)
    t4 should equal (Deep(One(f), Empty[({ type N[U, V] = Node[R, U, V] })#N, Float](), Two(f2, f3)))
    println("T4: " + t4)
  }


  "TFingerTree" should "addAllL" in {
    import ZList._

    type R[A, B] = A => Id[B]

    val f1 = (i:Double) => i.toFloat
    val f2 = (i:Float) => i.toInt
    val f3 = (i:Int) => i.toString

    val t = TFingerTree.empty[R, String]()
    val l = :::(f1, :::(f2, :::(f3, ZNil[R, String]())))

    val t2 = TFingerTree.addAllL(l, t)
    t2 should equal (Deep(Two(f1, f2), Empty[({ type N[U, V] = Node[R, U, V] })#N, Int](), One(f3)))

    println("T2:"+t2)

  }

  "TFingerTree" should "addAllR" in {
    import ZList._

    type R[A, B] = A => Id[B]

    val f1 = (i:Double) => i.toFloat
    val f2 = (i:Float) => i.toInt
    val f3 = (i:Int) => i.toString

    val t = TFingerTree.empty[R, Double]()
    val l = :::(f1, :::(f2, :::(f3, ZNil[R, String]())))

    val t2 = TFingerTree.addAllR(t, l)
    t2 should equal (Deep(One(f1), Empty[({ type N[U, V] = Node[R, U, V] })#N, Float](), Two(f2, f3)))

    println("T2:"+t2)

  }

  "TFingerTree" should "Digit.fromList" in {
    import ZList._

    type R[A, B] = A => Id[B]

    val f1 = (i:Double) => i.toFloat
    val f2 = (i:Float) => i.toInt
    val f3 = (i:Int) => i.toString

    val l = :::(f1, :::(f2, :::(f3, ZNil[R, String]())))
    println("l:"+l)
    val d = Digit.fromList(l)
    println("D:"+d)
    val l2 = Digit.toList(d)
    println("L2:"+l2)

    l2 should equal (l)
  }
}