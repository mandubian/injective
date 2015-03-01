/*
 * Copyright 2014 Pascal Voitot (@mandubian)
 *
 */
import scalaz._
import Scalaz._

/** Portage of @runorama code sample https://gist.github.com/runarorama/a8fab38e473fafa0921d to pure Scalaz */
object Injective {

  /** extends Natural Trans with "or" for composite interpreter */
  implicit class RichNat[F[_],G[_]](val nat: F ~> G) extends AnyVal {
    def or[H[_]](f: H ~> G): ({ type f[x] = Coproduct[F, H, x]})#f ~> G =
      new (({type f[x] = Coproduct[F,H,x]})#f ~> G) {
        def apply[A](c: Coproduct[F,H,A]): G[A] = c.run match {
          case -\/(fa) => nat(fa)
          case \/-(ha) => f(ha)
        }
      }
  }

  def lift[F[_], G[_], A](f: F[A])(implicit I: Inject[F, G]): Free.FreeC[G, A] = {
    Free.liftFC(I.inj(f))
  }

}

object App {
  import Injective._

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

  class Interacts[F[_]](implicit I: Inject[Interact, F]) {
    def tell(msg: String): Free.FreeC[F, Unit] = lift(Tell(msg))

    def ask(prompt: String): Free.FreeC[F, String] = lift(Ask(prompt))
  }

  object Interacts {
    implicit def instance[F[_]](implicit I: Inject[Interact, F]): Interacts[F] = new Interacts[F]
  }


  /**
    * Auth ADT
    */
  sealed trait Auth[A]
  case class Login(u: UserID, p: Password) extends Auth[Option[User]]
  case class HasPermission(u: User, p: Permission) extends Auth[Boolean]

  class Auths[F[_]](implicit I: Inject[Auth, F]) {
    def login(id: UserID, pwd: Password): Free.FreeC[F, Option[User]] = lift(Login(id, pwd))

    def hasPermission(u: User, p: Permission): Free.FreeC[F, Boolean] = lift(HasPermission(u, p))
  }

  object Auths {
    implicit def instance[F[_]](implicit I: Inject[Auth, F]): Auths[F] = new Auths[F]
  }


  val KnowSecret = "KnowSecret"

  def prg[F[_]](implicit I: Interacts[F], A: Auths[F]) = {
    import I._; import A._
    // Alias to help compiler for Free
    type CoyoF[T] = ({type f[x] = Coyoneda[F, x]})#f[T]

    for {
      uid <- ask("What's your user ID?")
      pwd <- ask("Password, please.")
      u   <- login(uid, pwd)
      b   <- u.map(hasPermission(_, KnowSecret)).getOrElse(Free.point[CoyoF, Boolean](false))
      _   <- if (b) tell("UUDDLRLRBA") else tell("Go away!")
    } yield ()
  }

  type App[A] = Coproduct[Auth, Interact, A]

  val app: Free.FreeC[App, Unit] = prg[App]

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

  object Console extends (Interact ~> Id) {
    def apply[A](i: Interact[A]) = i match {
      case Ask(prompt) =>
        println(prompt)
        readLine
      case Tell(msg) =>
        println(msg)
    }
  }

  // Compiler needs help here too
  val interpreters: App ~> Id = TestAuth or Console
  val coyoint: ({type f[x] = Coyoneda[App, x]})#f ~> Id = Coyoneda.liftTF(interpreters)
  def runApp = app.mapSuspension(coyoint)

}



  // type Tester[A] =
  //   Map[String, String] => (List[String], A)

  // object TestConsole extends (Interact ~> Tester) {
  //   def apply[A](i: Interact[A]) = i match {
  //     case Ask(prompt) => m => (List(), m(prompt))
  //     case Tell(msg) => _ => (List(msg), ())
  //   }
  // }

  // implicit def testerMonad: Monad[Tester] = new Monad[Tester]  {
  //   override def bind[A, B](t: Tester[A])(f: A => Tester[B]) =
  //     m => {
  //       val (o1, a) = t(m)
  //       val (o2, b) = f(a)(m)
  //       (o1 ++ o2, b)
  //     }
  //   override def point[A](a: => A) = _ => (List(), a)
  // }
