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
    val t2 = testTime("addAllL1") {
      addAllL(2 :: false :: "tata" :: 1.234 :: HNil, t1)
    }
    println(t2)
  }
}