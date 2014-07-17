/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
import org.scalatest._

import scalaz.{Free, Coyoneda}

import scala.concurrent._

class MacroSpec extends FlatSpec with Matchers {
  import shapeless._
  import poly._
  import ops.coproduct.{Inject, Selector}
  import Shapoyo._
  import ADT._
  import Interpreters._


  "ShapeApp" should "run second app" in {
    // APP DEFINITION
    type App[A] = Interact[A] :+: LogA[A] :+: Storage[String, Int, A] :+: CNil
    type CoyoApp[A] = Coyoneda[App, A]
    type FreeApp[A] = Free.FreeC[App, A]

    implicit val functor: scalaz.Monad[FreeApp] = Free.freeMonad[CoyoApp]
    implicitly[scalaz.Functor[FreeApp]]
    implicit def CopoyoApp[F[_], A](f: F[A])(implicit inj: Inject[App[A], F[A]]) = Copoyo[App](f)

    def parse(cmd: String) = {
      if     (cmd == "put") Copoyo[App](Put("alpha", 5): StorageSI[Unit])
      else if(cmd == "get") Copoyo[App](Get("alpha"): StorageSI[Int])
      else if(cmd == "end") Copoyo[App](EndI)
      else                  Copoyo[App](Log(ErrorLevel, s"Unknown Command: $cmd"))
    }

    // THE PROGRAM
    // val a = FreeCompile.compile {
    //   for {
    //     key   <- Copoyo[App](Ask("Enter key?"):   Interact[String])
    //     value <- Copoyo[App](Ask("Enter value?"): Interact[String])
    //     _     <- if(key != "") Copoyo[App](Put(key, value): StorageSI[Unit]) else Copoyo[App](Tell("bad key"))
    //   } yield ()
    // }
/*
    abstract class Cmd {
      type Out
      type F[_]

      val payload: F[Out]
      def exec[M[_]](natf: F ~> M): M[Out] = natf(payload)
    }

    class CmdPut(key: String, value: String) extends Cmd {
      type Out = Unit
      type F = Put

      val payload = Put(key, value)
    }

    val keyAsk = Ask("Enter key?")
    val key: Id[String] = Console(keyAsk)
    val valueAsk = Ask("Enter value?")
    val value: Id[String] = Console(valueAsk)
    val _Put = if(key != "") CmdPut(key, value).exec(Store)
    val idCmd = Console(cmd)
*/
    // val interpreters: App ~> Id = Console ||: Logger ||: Store

    // val lis: CoyoApp ~> Id = liftCoyoLeft(interpreters)
    // println("RESULT:"+prg.mapSuspension(lis).foldMap(scalaz.NaturalTransformation.id))
    // println("store:"+store)
  }

}