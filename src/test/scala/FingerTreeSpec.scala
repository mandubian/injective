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

trait Instrumented {
  def testTime[A](name: String)(body: => A): A = {
    val t1 = System.currentTimeMillis()
    val r = body
    val t2 = System.currentTimeMillis()
    println( "For " + name + " the tests took " + formatSeconds( (t2 - t1) * 0.001 ))
    r
  }

  def formatSeconds( seconds: Double ) : String = {
    val millisR    = (seconds * 1000).toInt
    val sb         = new StringBuilder( 10 )
    val secsR      = millisR / 1000
    val millis     = millisR % 1000
    val mins       = secsR / 60
    val secs       = secsR % 60
    if( mins > 0 ) {
       sb.append( mins )
       sb.append( ':' )
       if( secs < 10 ) {
          sb.append( '0' )
       }
    }
    sb.append( secs )
    sb.append( '.' )
    if( millis < 10 ) {
       sb.append( '0' )
    }
    if( millis < 100 ) {
       sb.append( '0' )
    }
    sb.append( millis )
    sb.append( 's' )
    sb.toString()
  }
}


class FingerTreeSpec extends FlatSpec with Matchers with Instrumented {
  import shapeless._
  import poly._
  import FingerTree._

  it should "prepend/deepl" in {
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
  }

  it should "addAllL" in {
    import FingerTree._

    val t = FingerTree.empty()

    val a1 = implicitly[FingerTree.AddAllL.Case.Aux[HNil, Empty.type, Empty.type]]
    val a2 = implicitly[FingerTree.AddAllL.Case.Aux[Boolean :: HNil, Empty.type, Single[Boolean]]]
    val a3 = implicitly[FingerTree.Prepend.Case[Boolean, t.type]]

    val t1 = testTime("addAllL1") {
      addAllL(1 :: "tutu" :: true :: 34 :: HNil, t)
    }

    t1 should equal (Deep(Digit.Three(1, "tutu", true), FingerTree.empty(), Digit.One(34)))

    val t2 = testTime("addAllL2") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t1)
    }
    println("ADDALLL T2:"+t2)
    val t3 = testTime("addAllL3") {
      addAllL(1 :: 2 :: 3 :: "toto" :: HNil, t2)
    }

    println("ADDALLL  T3:"+t3)

    val t4 = testTime("addAllL4") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t3)
    }
    val t5 = testTime("addAllL5") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t4)
    }
    val t6 = testTime("addAllL6") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t5)
    }
    val t7 = testTime("addAllL7") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t6)
    }
    val t8 = testTime("addAllL8") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t7)
    }
    val t9 = testTime("addAllL9") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t8)
    }
    val t10 = testTime("addAllL10") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t9)
    }
    val t11 = testTime("addAllL11") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t10)
    }
    val t12 = testTime("addAllL12") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t11)
    }
    val t13 = testTime("addAllL13") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t12)
    }
    val t14 = testTime("addAllL14") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t13)
    }

    println("ADDALLL T14:"+t14)
  }

  it should "addAllR" in {
    import FingerTree._

    val t = FingerTree.empty()

    val a1 = implicitly[FingerTree.AddAllR.Case.Aux[Empty.type, HNil, Empty.type]]
    val a2 = implicitly[FingerTree.AddAllR.Case.Aux[Empty.type, Boolean :: HNil, Single[Boolean]]]
    val a3 = implicitly[FingerTree.Prepend.Case[Boolean, t.type]]

    val t1 = testTime("addAllR1") {
      addAllR(t, 1 :: "tutu" :: true :: 34 :: HNil)
    }

    println("ADDALLR T1:"+t1)
    t1 should equal (Deep(Digit.One(1), FingerTree.empty(), Digit.Three("tutu", true, 34)))

    val t2 = testTime("addAllR2") {
      addAllR(t1, 2 :: false :: "tata" :: 1.234 :: HNil)
    }
    println("ADDALLR T2:"+t2)
    val t3 = testTime("addAllR3") {
      addAllR(t2, 1 :: 2 :: 3 :: "toto" :: HNil)
    }

    println("ADDALLR T3:"+t3)

    val t4 = testTime("addAllR4") {
      addAllR(t3, 2 :: false :: "tata" :: 1.234 :: HNil)
    }
    val t5 = testTime("addAllR5") {
      addAllR(t4, 2 :: false :: "tata" :: 1.234 :: HNil)
    }
    val t6 = testTime("addAllR6") {
      addAllR(t5, 2 :: false :: "tata" :: 1.234 :: HNil)
    }
    val t7 = testTime("addAllR7") {
      addAllR(t6, 2 :: false :: "tata" :: 1.234 :: HNil)
    }
    val t8 = testTime("addAllR8") {
      addAllR(t7, 2 :: false :: "tata" :: 1.234 :: HNil)
    }
    val t9 = testTime("addAllR9") {
      addAllR(t8, 2 :: false :: "tata" :: 1.234 :: HNil)
    }
    val t10 = testTime("addAllR10") {
      addAllR(t9, 2 :: false :: "tata" :: 1.234 :: HNil)
    }
    val t11 = testTime("addAllR11") {
      addAllR(t10, 2 :: false :: "tata" :: 1.234 :: HNil)
    }
    val t12 = testTime("addAllR12") {
      addAllR(t11, 2 :: false :: "tata" :: 1.234 :: HNil)
    }
    val t13 = testTime("addAllR13") {
      addAllR(t12, 2 :: false :: "tata" :: 1.234 :: HNil)
    }
    val t14 = testTime("addAllR14") {
      addAllR(t13, 2 :: false :: "tata" :: 1.234 :: HNil)
    }

    println("ADDALLR T14:"+t14)

  }

  it should "prepend/append/add ops" in {
    val t1 = 1 +: "toto" +: true +: 1.234 +: FingerTree.Empty
    println("T1:"+t1)
    val t2 = FingerTree.Empty :+ 1 :+ "toto" :+ true :+ 1.234
    println("T2:"+t2)

    val t3 = t1 ++ t2
  }

  it should "add" in {
    val t1 = 1 +: "toto" +: true +: 1.234 +: FingerTree.Empty
    val t2 = FingerTree.Empty :+ 1 :+ "toto" :+ true :+ 1.234
    val t3 = t1 ++ t2

    println("T3:"+t3)
    t3 should equal (
      Deep(
        Digit.Three(1, "toto", true),
        Single(Node.Node2(1.234, 1)),
        Digit.Three("toto", true, 1.234)
      )
    )

    val t4 = testTime("add4") {
      t3 ++ t3
    }

    val t5 = testTime("add5") {
      t4 ++ t3
    }

    val t6 = testTime("add6") {
      t5 ++ t3
    }

    val t7 = testTime("add7") {
      t6 ++ t3
    }

    val t8 = testTime("add8") {
      t7 ++ t3
    }

    val t9 = testTime("add9") {
      t8 ++ t3
    }

    val t10 = testTime("add10") {
      t9 ++ t3
    }

    val t11 = testTime("add11") {
      t10 ++ t3
    }

    val t12 = testTime("add12") {
      t11 ++ t3
    }

    val t13 = testTime("add13") {
      t12 ++ t3
    }

    println("T13:"+t13)
  }
}