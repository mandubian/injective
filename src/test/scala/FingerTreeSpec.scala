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

class FingerTreeSpec extends FlatSpec with Matchers {
  import shapeless._
  import poly._
  import FingerTree._

  "FingerTree" should "work" in {
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

}