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

object ADT {
  /**
    * Type Aliases
    */
  type UserID = String
  type Password = String
  type Permission = String
  case class User(id: String)

  /**
    * User Interaction ADT
    */
  sealed trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]

  sealed trait Auth[A]
  case class Login(u: UserID, p: Password) extends Auth[Option[User]]
  case class HasPermission(u: User, p: Permission) extends Auth[Boolean]

  sealed trait LogLevel
  case object ErrorLevel extends LogLevel
  case object WarnLevel extends LogLevel
  case object InfoLevel extends LogLevel
  case object DebugLevel extends LogLevel

  case class Log[A](level: LogLevel, msg: String)

  sealed trait Storage[K, V, A]
  case class Get[K, V](key: K) extends Storage[K, V, V]
  case class Put[K, V](key: K, value: V) extends Storage[K, V, Unit]
  case class Del[K, V](key: K) extends Storage[K, V, Unit]
}

class ShapelessSpec extends FlatSpec with Matchers {
  import shapeless._
  import poly._
  import ops.coproduct.{Inject, Selector}
  import Shapoyo._
  import ADT._

  "ShapeApp" should "run 1st App" in {

    // APP DEFINITION
    type App[A]     = Interact[A] :+: Auth[A] :+: CNil
    type CoyoApp[A] = Coyoneda[App, A]

    // THE HELPERS
    object interacts {
      def ask(prompt: String) = Copoyo[App](Ask(prompt))
      def tell(msg: String) = Copoyo[App](Tell(msg))
    }

    object auths {
      def login(u: UserID, p: Password) = Copoyo[App](Login(u, p))
      def hasPermission(u: User, p: Permission) = Copoyo[App](HasPermission(u, p))
    }

    val KnowSecret = "KnowSecret"

    // THE PROGRAM
    val prg = {
      import interacts._, auths._
      for {
        uid <- ask("What's your user ID?")
        pwd <- ask("Password, please.")
        u   <- login(uid, pwd)
        b   <- u map (hasPermission(_, KnowSecret)) getOrElse (Free.Return[CoyoApp, Boolean](false))
        _   <- if (b) tell("UUDDLRLRBA") else tell("Go away!")
      } yield ()
    }

    // THE INTERPRETERS
    object Console extends (Interact ~> Id) {
      def apply[A](i: Interact[A]) = i match {
        case Ask(prompt) =>
          println(prompt)
          readLine
        case Tell(msg) =>
          println(msg)
      }
    }

    object TestAuth extends (Auth ~> Id) {
      def apply[A](a: Auth[A]) = a match {
        case Login(uid, pwd) =>
          if (uid == "john.snow" && pwd == "Ghost")
            Some(User("john.snow"))
          else None
        case HasPermission(u, _) =>
          u.id == "john.snow"
      }
    }


    // THE EXECUTION
    val interpreters: App ~> Id = Console or TestAuth
    val lis: CoyoApp ~> Id = liftCoyoLeft(interpreters)
    prg.mapSuspension(lis)

  }

}