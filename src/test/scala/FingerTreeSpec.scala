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

// object MyApp {
//   val metricRegistry = new com.codahale.metrics.MetricRegistry()
// }

// trait Instrumented extends InstrumentedBuilder {
//   val metricRegistry = MyApp.metricRegistry
// }

class FingerTreeSpec extends FlatSpec with Matchers with Instrumented {
  import shapeless._
  import poly._
  import FingerTree._

  /*"FingerTree" should "work" in {
    val t = FingerTree.empty()
    println("Tree:"+t)
    val t2 = FingerTree.deep(Digit.One(5), t, Digit.One("toto"))
    println("Tree2:"+t2)

    val t3 = prepend(5, t)
    println("T3: " + t3)
    val t4 = prepend("tutu", t3)
    println("T4: " + t4)
    val t5 = prepend(true, t4)
    println("T5: " + t5)

    val t6 = deepl(5 :: "tutu" :: true :: HNil, t, Digit.One(2.34))
    println("T6: " + t6)
  }*/

  "FingerTree" should "add" in {
    import FingerTree._

    val t = FingerTree.empty()

    //val a1 = implicitly[FingerTree.AddAllL.Case.Aux[HNil, Empty.type, Empty.type]]
    //val a2 = implicitly[FingerTree.AddAllL.Case.Aux[Boolean :: HNil, Empty.type, Single[Boolean]]]
    //val a3 = implicitly[FingerTree.Prepend.Case[Boolean, t.type]]

    val t1 = testTime("addAllL1") {
      addAllL(1 :: "tutu" :: true :: HNil, t)
    }
    t1 should equal (Deep(Digit.Two(1, "tutu"), FingerTree.empty(), Digit.One(true)))
    val t2 = testTime("addAllL2") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t1)
    }
    println("T2:"+t2)
    // val t3 = testTime("addAllL3") {
    //   addAllL(1 :: 2 :: 3 :: 4 :: HNil, t2)
    // }

    implicitly[ 
      FingerTree.Prepend.Case[
        Int,
        FingerTree.Deep[
          FingerTree.Digit.Two[Int,Int],
          FingerTree.Deep[
            FingerTree.Digit.One[FingerTree.Node.Node3[Int,Boolean,String]],
            FingerTree.Empty.type,
            FingerTree.Digit.One[FingerTree.Node.Node3[Double,Int,String]]
          ],
          FingerTree.Digit.One[Boolean]
        ]
      ]
    ]

    // val t4 = testTime("addAllL4") {
    //   addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t3)
    // }
    // val t5 = testTime("addAllL5") {
    //   addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t4)
    // }
    // val t6 = testTime("addAllL6") {
    //   addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t5)
    // }
    // val t7 = testTime("addAllL7") {
    //   addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t6)
    // }
    // val t8 = testTime("addAllL8") {
    //   addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t7)
    // }
    // val t9 = testTime("addAllL9") {
    //   addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t8)
    // }
    // val t10 = testTime("addAllL10") {
    //   addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t9)
    // }
    // val t11 = testTime("addAllL11") {
    //   addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t10)
    // }
    // val t12 = testTime("addAllL12") {
    //   addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t11)
    // }
    // val t13 = testTime("addAllL13") {
    //   addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t12)
    // }
    // val t14 = testTime("addAllL14") {
    //   addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t13)
    // }
  }
}