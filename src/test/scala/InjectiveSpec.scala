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

class InjectiveSpec extends FlatSpec with Matchers {
  import shapeless._
  import poly._
  import ops.coproduct.{Inject, Selector}
  import Shapoyo._
  import ADT._
  import Interpreters._

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



  "ShapeApp" should "run 3rd app" in {
    // APP DEFINITION
    type App[A] = FileSystem[A] :+: LogA[A] :+: CNil
    type CoyoApp[A] = Coyoneda[App, A]
    type FreeApp[A] = Free.FreeC[App, A]

    // THE PROGRAM
    def prg: FreeApp[Unit] =
      for {
        line <- Copoyo[App](ReadLine)
        _    <- Copoyo[App](Log(InfoLevel, "read "+line))
        _    <- line match {
                  case Some(line) => Copoyo[App](PutLine(line)) flatMap ( _ => prg )
                  case None       => Copoyo[App](Eof)
                }
      } yield ()

    val interpreters: App ~> Id = File ||: Logger
    val lis: CoyoApp ~> Id = liftCoyoLeft(interpreters)

    println("RESULT:"+prg.foldMap(lis))
  }














    // type App1[A] = Interact[A] :+: LogA[A] :+: Storage[String, Int, A] :+: CNil

    // object Program1 extends Program[App1] {
    //   implicit val prg = for {
    //     cmd <- Ask("Enter command?")
    //     _   <- Log(DebugLevel, s"Read Command: $cmd")
    //   } yield ()
    // }

    // val i1: App1 ~> Id = Console or Logger
    // val i2 = i1 or Store
    // val interpreters: App1 ~> Id = (Console or Logger) or Store
    // Program1.exec(Console or Logger)

    // implicit object polyInteract extends Poly1 {
    //   implicit def caseInteract[A] = at[Interact[A]] { i => Console(i) }
    // }

    // object polyAuth extends Poly1 {
    //   implicit def caseAuth[A] = at[Auth[A]] { a => TestAuth(a) }
    // }

    // val p = polyInteract |+| polyAuth

    // trait Cup1[-P, F[_ <: Coproduct], U <: Coproduct]

    // object Cup1 {
    //   implicit def cup1[F[_ <: Coproduct], U <: Coproduct] = 
    //     new Cup1[F[U], F, U] {}
    // }

    // trait Cup2[-P, T, U <: Coproduct]

    // object Cup2 {
    //   implicit def cup2[:+:[_, _ <: Coproduct], T, U <: Coproduct] = 
    //     new Cup2[:+:[T, U], T, U] {}
    // }

    // def f[P <: Poly, C <: Coproduct, FF, C1 <: Coproduct, F[_], A, L <: HList, MR](p: P)(cop: C)(
    //   implicit  cup2:   Cup2[C, FF, C1],
    //             unapp1: Unapply1[FF, F, A],
    //             c:      Case.Aux[p.type, L, MR],
    //             ev:     (F[A]::HNil) =:= L
    // ) = c

    // class PolyCop[C <: Coproduct] extends Poly
    // object PolyCop {

      // implicit def polyCop[C <: Coproduct, A, P <: Poly1, MR](
      //   implicit unpack2:  Cup2[C,  A, CNil],
      //            c1 : Case.Aux[P, A, MR]
      // ) = new PolyCop[C]

      // implicit def polyCop[C <: Coproduct, C1 <: Coproduct, A, B, P <: Poly, Q <: Poly, MR, MR2](
      //   implicit unpack2:  Cup2[C,  A, C1],
      //            unpack21: Cup2[C1, B, CNil],
      //            c1 : Case.Aux[P, A::HNil, MR],
      //            c2 : Case.Aux[Q, B::HNil, MR2]
      // ) = new PolyCop[C]

      // implicit def cop[C <: Coproduct, P <: Poly](
      //   implicit unpack2: Cup2[C,  P, CNil]
      // ) = new PolyCop[C]

      // implicit def cop2[C <: Coproduct, P <: Poly, C1 <: Coproduct](
      //   implicit unpack2: Cup2[C,  P, C1],
      //            polyCop: PolyCop[C1]
      // ) = new PolyCop[C]

      // implicit def polyCop1[PC, C <: Coproduct, P <: Poly, ML <: HList, MR](
      //   implicit unpack1: Cup1[PC, PolyCop, C],
      //            unpack2: Cup2[C,  P, CNil],
      //            c : Case.Aux[P, ML, MR]
      // ) = new Case[C, ML] {
      //   type Result = MR
      //   val value = (t : ML) => c(t)
      // }

      // implicit def polyCop21[PC, C <: Coproduct, C1 <: Coproduct, P <: Poly, Q <: Poly, ML <: HList, MR](
      //   implicit unpack1: Cup1[PC, PolyCop, C],
      //            unpack2:  Cup2[C,  P, C1],
      //            unpack21: Cup2[C1, Q, CNil],
      //            c : Case.Aux[P, ML, MR]
      // ) = new Case[C, ML] {
      //   type Result = MR
      //   val value = (t : ML) => c(t)
      // }

      // implicit def polyCop22[C <: Coproduct, C1 <: Coproduct, P <: Poly, Q <: Poly, ML <: HList, MR](
      //   implicit unpack2:  Cup2[C,  P, C1],
      //            unpack21: Cup2[C1, Q, CNil],
      //            c : Case.Aux[Q, ML, MR]
      // ) = new Case[C, ML] {
      //   type Result = MR
      //   val value = (t : ML) => c(t)
      // }
    // }

    // implicit def f[C <: Coproduct, P <: Poly, C1 <: Coproduct, F1, G1, F[_], G[_], A, ML <: HList, MR](
    //   implicit unpack2:  Cup2[C,  F1, C1],
    //            unpack1:  Unpack1[F1, F, A],
    //            unpack21: Cup2[C1, G1, CNil],
    //            unpack11: Unpack1[G1, G, A],
    //            c : Case.Aux[P, ML, MR]
    // ) = new Case[C, ML] {
    //   type Result = MR
    //   val value = (t : ML) => c(t)
    // }

    //def l[A] = implicitly[Case.Aux[polyInteract, Interact[A] :: HNil, Id[A]]]
    // type App[A] = Interact[A] :+: Auth[A] :+: CNil
    // type TPE = polyInteract.type :+: polyAuth.type :+: CNil
    // PolyCop.polyCop[
    //   Interact[String] :+: Auth[String] :+: CNil,
    //   Auth[String] :+: CNil,
    //   polyInteract.type,
    //   polyAuth.type,
    //   Interact[String],
    //   Auth[String],
    //   Id[String],
    //   Id[String]
    // ]
    //val pc = implicitly[PolyCop[polyInteract.type :+: polyAuth.type :+: CNil]]
    //val pc = implicitly[PolyCop[Interact[String] :+: CNil]]
    //implicitly[Case.Aux[polyAuth.type, Auth[String]::HNil, Id[String]]]
    //implicitly[Cup1[pc.type, PolyCop, TPE]]
    //pc(Coproduct[App[String]](Ask("toto"):Interact[String]))
    //PolyCop.polyCop21[pc.type, polyInteract.type :+: polyAuth.type :+: CNil, polyAuth.type :+: CNil, polyInteract.type, polyAuth.type, Interact[String] :: HNil, Id[String]]
    //implicitly[Case.Aux[polyInteract.type, Interact[String] :: HNil, Id[String]]]
    //implicitly[Cup2[Interact[String] :+: Auth[String] :+: CNil, Interact[String], polyAuth.type :+: CNil]]
    //implicitly[Cup2[polyAuth.type :+: CNil, polyAuth.type, CNil]]


  "ShapeApp" should "run merge" in {
    object size extends Poly1 {
      implicit def caseInt = at[Int] { i => 1 }
      implicit def caseString = at[String] { s => s.size }
    }

    object size2 extends Poly1 {
      implicit def caseTuple[T, U]
        (implicit st : size.Case.Aux[T, Int], su : size.Case.Aux[U, Int]) =
          at[(T, U)](t => size(t._1)+size(t._2))
    }

    object size3 extends Poly2 {
      implicit  def default[T, U](implicit st: size.Case.Aux[T, Int], su: size.Case.Aux[U, Int]) =
        at[T, U]{ (t, u) => size(t) + size(u) }
    }

    val m = Merge(Merge(size, size2), size3)
    m(5) should equal (1)
    m("toto") should equal (4)

    m(5 -> "toto") should equal (5)
    m(5, "toto") should equal (5)
  }
}