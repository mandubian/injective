/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
import org.scalatest._

import scalaz.{Free, Coyoneda}

import scala.concurrent._

class AppSpec extends FlatSpec with Matchers with Instrumented {
  import shapeless._
  import poly._
  import ops.coproduct.{Inject, Selector}
  import Shapoyo._
  import ADT._
  import Interpreters._


  "ShapeApp" should "Strict TFree" in {
    import strict._
    import TFree._

    // APP DEFINITION
    type App[A] = FileSystem[A] :+: LogA[A] :+: CNil
    type CoyoApp[A] = Coyoneda[App, A]
    type TFreeApp[A] = TFree.TFreeC[App, A]

    // THE PROGRAM
    def prg: TFreeApp[Unit] =
      // for {
      //   line <- TCopoyo[App](ReadLine)
      // //   // _ <- TCopoyo[App](Log(InfoLevel, "read "+line))
      //   _ <- line match {
      //           case Some(line) => prg //TCopoyo[App](PutLine(line)) flatMap ( _ => prg )
      //           case None       => TCopoyo[App](Eof)
      //         }
      // } yield ()
      TCopoyo[App](ReadLine) flatMap {
        case Some(line) => prg //TCopoyo[App](PutLine(line)) flatMap ( _ => prg )
        case None       => TCopoyo[App](Eof)
      } //map { _ => () }


    // val interpreters: App ~> Free.Trampoline = File2 ||: Logger2
    // val lis: CoyoApp ~> Free.Trampoline = liftCoyoLeft(interpreters)

    def buildInterpreter(n: Int): CoyoApp ~> TFree.Trampoline = {
      def interpreters: App ~> TFree.Trampoline = fileInterpreter2(n) ||: Logger3
      liftCoyoLeft(interpreters)
    }

    val testN = Seq[Int](
      // 1000, 5000
      // , 10000, 20000, 50000
      // , 100000
      100000000
    )

    println("Strict Fixed Free App")
    testN foreach { n =>
      testTime2(s"$n") { prg.foldMap(buildInterpreter(n)).run }
    }

  }

}