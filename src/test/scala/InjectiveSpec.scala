/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
import org.scalatest._

import scalaz.{Free, Coyoneda}

import scala.concurrent._

/*class InjectiveSpec extends FlatSpec with Matchers {

  "Injective" should "run the app" in {
    App.runApp
  }
}*/

class InjectiveSpec extends FlatSpec with Matchers with Instrumented {
  import shapeless._
  import poly._
  import ops.coproduct.{Inject, Selector}
  import Shapoyo._
  import ADT._
  import Interpreters._

/*
  "ShapeApp" should "run 3rd app 1" in {
    // APP DEFINITION
    type App[A] = FileSystem[A] :+: LogA[A] :+: CNil
    type CoyoApp[A] = Coyoneda[App, A]
    type FreeApp[A] = Free.FreeC[App, A]

    // THE PROGRAM
    def prg: FreeApp[Unit] =
      Copoyo[App](ReadLine) flatMap {
        case Some(line) => prg
        case None       => Copoyo[App](Eof)
      }

/*    def prg: FreeApp[Unit] =
      for {
        line <- Copoyo[App](ReadLine)
        _    <- Copoyo[App](Log(InfoLevel, "read "+line))
        _    <- line match {
                  case Some(line) => Copoyo[App](PutLine(line)) flatMap ( _ => prg )
                  case None       => Copoyo[App](Eof)
                }
      } yield ()*/

    def buildInterpreter(n: Int): CoyoApp ~> Free.Trampoline = {
      val interpreters: App ~> Free.Trampoline = fileInterpreter(n) ||: Logger2
      liftCoyoLeft(interpreters)
    }

    val testN = Seq[Int](
      1000, 5000
      , 10000, 20000, 50000
      , 100000
    )

    println("Scalaz Free App")
    testN foreach { n =>
      testTime2(s"$n") { prg.foldMap(buildInterpreter(n)).run }
    }

  }

*/
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


"ShapeApp" should "Lazy TFree" in {
    import `lazy`._
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
      def interpreters: App ~> TFree.Trampoline = fileInterpreter2Lazy(n) ||: Logger3Lazy
      liftCoyoLeft(interpreters)
    }

    val testN = Seq[Int](
      // 1000, 5000
      // , 10000, 20000, 50000
      100000000
    )

    println("Lazy Fixed Free App")
    testN foreach { n =>
      testTime2(s"$n") { prg.foldMap(buildInterpreter(n)).run }
    }

  }








  // "ShapeApp" should "run 1st App" in {

  //   // APP DEFINITION
  //   type App[A]     = Interact[A] :+: Auth[A] :+: CNil
  //   type CoyoApp[A] = Coyoneda[App, A]

  //   // THE HELPERS
  //   object interacts {
  //     def ask(prompt: String) = Copoyo[App](Ask(prompt))
  //     def tell(msg: String) = Copoyo[App](Tell(msg))
  //   }

  //   object auths {
  //     def login(u: UserID, p: Password) = Copoyo[App](Login(u, p))
  //     def hasPermission(u: User, p: Permission) = Copoyo[App](HasPermission(u, p))
  //   }

  //   val KnowSecret = "KnowSecret"

  //   // THE PROGRAM
  //   val prg = {
  //     import interacts._, auths._
  //     for {
  //       uid <- ask("What's your user ID?")
  //       pwd <- ask("Password, please.")
  //       u   <- login(uid, pwd)
  //       b   <- u map (hasPermission(_, KnowSecret)) getOrElse (Free.Return[CoyoApp, Boolean](false))
  //       _   <- if (b) tell("UUDDLRLRBA") else tell("Go away!")
  //     } yield ()
  //   }

  //   // THE EXECUTION
  //   //val interpreters: App ~> Id = Console or CoproductNat(TestAuth)
  //   //val lis: CoyoApp ~> Id = liftCoyoLeft(interpreters)
  //   //prg.mapSuspension(lis)

  // }

  // "ShapeApp" should "run second app" in {
  //   // APP DEFINITION
  //   type App[A] = Interact[A] :+: LogA[A] :+: Storage[String, Int, A] :+: CNil
  //   type CoyoApp[A] = Coyoneda[App, A]
  //   type FreeApp[A] = Free.FreeC[App, A]

  //   implicit val functor: scalaz.Monad[FreeApp] = Free.freeMonad[CoyoApp]
  //   implicitly[scalaz.Functor[FreeApp]]
  //   implicit def CopoyoApp[F[_], A](f: F[A])(implicit inj: Inject[App[A], F[A]]) = Copoyo[App](f)

  //   def parse(cmd: String) = {
  //     if     (cmd == "put") Copoyo[App](Put("alpha", 5): StorageSI[Unit])
  //     else if(cmd == "get") Copoyo[App](Get("alpha"): StorageSI[Int])
  //     else if(cmd == "end") Copoyo[App](EndI)
  //     else                  Copoyo[App](Log(ErrorLevel, s"Unknown Command: $cmd"))
  //   }

  //   // THE PROGRAM
  //   def prg: FreeApp[Unit] =
  //     for {
  //       cmd <- Ask("Enter command?")
  //       i   <- parse(cmd)
  //       _   <- if(i != End) prg else Copoyo[App](EndI)
  //     } yield ()

  //   val interpreters: App ~> Id = Console ||: Logger ||: Store
  //   val lis: CoyoApp ~> Id = liftCoyoLeft(interpreters)

  //   println("RESULT:"+prg.foldMap(lis))
  //   println("store:"+store)
  // }


}