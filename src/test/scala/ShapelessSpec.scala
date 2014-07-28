/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
import org.scalatest._

import scalaz.{Free, Coyoneda}

import scala.concurrent._

class ShapelessSpec extends FlatSpec with Matchers {
  import shapeless._
  import poly._
  import ShapelessExt._


  "ShapelessExt" should "try" in {
    def sum[T, TZ <: T, A <: T, B <: T](a: A, b: B)(implicit hm: HMonoid[T, TZ]) = hm.append(a, b)

  }

}