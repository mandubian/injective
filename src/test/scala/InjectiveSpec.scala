/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
import org.scalatest._

import scalaz._
import Scalaz._
import scala.concurrent._

class InjectiveSpec extends FlatSpec with Matchers {

  "Injective" should "run the app" in {
    App.runApp
  }
}


