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

    try {
      //println("RESULT:"+prg.mapSuspension(lis).run)
      testTime("Scalaz Free 1000") { prg.foldMap(buildInterpreter(1000)).run }
      // testTime("TFree 2000") { prg.foldMap(buildInterpreter(2000)).run }
      // testTime("TFree 3000") { prg.foldMap(buildInterpreter(3000)).run }
      // testTime("TFree 4000") { prg.foldMap(buildInterpreter(4000)).run }
      // testTime("TFree 5000") { prg.foldMap(buildInterpreter(5000)).run }
      // testTime("Scalaz Free 10000") { prg.foldMap(buildInterpreter(10000)).run }
      // testTime("Scalaz Free 20000") { prg.foldMap(buildInterpreter(20000)).run }
      // testTime("Scalaz Free 30000") { prg.foldMap(buildInterpreter(30000)).run }
      // testTime("Scalaz Free 40000") { prg.foldMap(buildInterpreter(40000)).run }
      // testTime("Scalaz Free 50000") { prg.foldMap(buildInterpreter(50000)).run }
      // testTime("Scalaz Free 100000") { prg.foldMap(buildInterpreter(100000)).run }
      // testTime("TFree 500000") { prg.foldMap(buildInterpreter(500000)).run }
      testTime("Scalaz Free 50000") { println("RES:"+prg.foldMap(buildInterpreter(50000)).run) }
    } catch {
      case e:Throwable => e.printStackTrace
    }
  }


  "ShapeApp" should "run 3rd app TFree" in {
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

    try {
      testTime("Fixed Free 1000") { prg.foldMapT(buildInterpreter(1000)).run }
      // testTime("Fixed Free 1000000")  { prg.foldMap(buildInterpreter(1000000)).run }
      // testTime("Fixed Free 2000000")  { prg.foldMap(buildInterpreter(2000000)).run }
      // testTime("Fixed Free 3000000")  { prg.foldMap(buildInterpreter(3000000)).run }
      // testTime("Fixed Free 4000000")  { prg.foldMap(buildInterpreter(4000000)).run }
      testTime("Fixed Free 50000 FoldMapT")  { println("RES:"+prg.foldMapT(buildInterpreter(50000)).run) }
      // testTime("Fixed Free 1000000 FoldMapT")  { println("RES:"+prg.foldMapT(buildInterpreter(1000000)).run) }
      // testTime("Fixed Free 1000000 FoldMapT")  { println("RES:"+prg.foldMapT(buildInterpreter(1000000)).run) }
      // testTime("Fixed Free 1000000 FoldMapT")  { println("RES:"+prg.foldMapT(buildInterpreter(1000000)).run) }
      // testTime("Fixed Free 1000000 FoldMapT")  { println("RES:"+prg.foldMapT(buildInterpreter(1000000)).run) }
      // testTime("Fixed Free 1000000 FoldMapT")  { println("RES:"+prg.foldMapT(buildInterpreter(1000000)).run) }
      // testTime("Fixed Free 1000000 FoldMap ")  { println("RES:"+prg.foldMap (buildInterpreter(1000000)).run) }
      // testTime("Fixed Free 1000000 FoldMap ")  { println("RES:"+prg.foldMap (buildInterpreter(1000000)).run) }
      // testTime("Fixed Free 1000000 FoldMap ")  { println("RES:"+prg.foldMap (buildInterpreter(1000000)).run) }
      // testTime("Fixed Free 1000000 FoldMap ")  { println("RES:"+prg.foldMap (buildInterpreter(1000000)).run) }
      // testTime("Fixed Free 1000000 FoldMap ")  { println("RES:"+prg.foldMap (buildInterpreter(1000000)).run) }
      // testTime("Fixed Free 1000000 FoldMap ")  { println("RES:"+prg.foldMap (buildInterpreter(1000000)).run) }
      // testTime("Fixed Free 10000000") { prg.foldMap(buildInterpreter(10000000)).run }
      // testTime("Fixed Free 20000000") { prg.foldMap(buildInterpreter(20000000)).run }
      // testTime("Fixed Free 400000") { prg.foldMapT(buildInterpreter(400000)).run }
      // testTime("Fixed Free 800000") { prg.foldMapT(buildInterpreter(800000)).run }
      // testTime("Fixed Free 1000000") { prg.foldMapT(buildInterpreter(1000000)).run }

    } catch {
      case e:Throwable => e.printStackTrace
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