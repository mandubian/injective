/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
import org.scalatest._

import scalaz.{Free, Coyoneda, \/, -\/, \/-, Trampoline, ~>}

import scala.concurrent._

class AppSpec extends FlatSpec with Matchers with Instrumented {
  import shapeless._
  import ops.coproduct.{Inject, Selector}
  import Shapoyo._

  "ShapeApp" should "Strict TFree" in {
    import Free._

    // Global ADT
    sealed trait LogLevel
    case object ErrorLevel extends LogLevel
    case object WarnLevel extends LogLevel
    case object InfoLevel extends LogLevel
    case object DebugLevel extends LogLevel

    trait Log[A]
    case class LogMsg(level: LogLevel, msg: String) extends Log[Unit]


    //////////////////////////////////////////////////////////////////////////
    // DB Interaction Application
    object DB {
      // DB ADT
      type Entity = Map[String, String]

      sealed trait DBError 
      case object NotFound extends DBError

      sealed trait DBInteract[A]
      case class FindById(id: String) extends DBInteract[DBError \/ Entity]

      // APP DEFINITION
      type App[A] = DBInteract[A] :+: Log[A] :+: CNil
      type CoyoApp[A] = Coyoneda[App, A]
      type FreeApp[A] = Free.FreeC[App, A]

      // HELPERS
      object Log {
        def debug(msg: String) = Copoyo[App](LogMsg(DebugLevel, msg))
      }

      def findById(id: String): FreeApp[DBError \/ Entity] = 
        for {
          _    <- Log.debug("Searching for entity id:"+id)
          res  <- Copoyo[App](FindById(id))
          _    <- Log.debug("Search result:"+res)
        } yield (res)
    }

    //////////////////////////////////////////////////////////////////////////
    // Http Server
    object Http {

      // Http ADT
      sealed trait  HttpVerb
      case object   Get extends HttpVerb
      case object   Post extends HttpVerb

      sealed trait  HttpStatus                              { val value: Int  }
      case object   Ok                  extends HttpStatus  { val value = 200 }
      case object   BadRequest          extends HttpStatus  { val value = 400 }
      case object   InternalServerError extends HttpStatus  { val value = 500 }

      type Params = Map[String, Seq[String]]
      type Headers = Map[String, Seq[String]]

      sealed trait HttpReq {
        val verb: HttpVerb
        val url: String
        val params: Params
        val headers: Headers
      }

      case class GetReq(
        url: String,
        params: Params = Map.empty[String, Seq[String]],
        headers: Headers = Map.empty[String, Seq[String]]
      ) extends HttpReq {
        val verb = Get
      }

      case class PostReq(
        url: String,
        params: Params = Map.empty[String, Seq[String]],
        headers: Headers = Map.empty[String, Seq[String]],
        body: String
      ) extends HttpReq {
        val verb = Post
      }

      case class HttpResp (
        status: HttpStatus,
        headers: Headers = Map.empty[String, Seq[String]],
        body: String = ""
      )

      sealed trait  RecvError
      case object   ClientDisconnected extends RecvError
      case object   Timeout extends RecvError

      sealed trait  SendStatus
      case object   Ack extends SendStatus
      case object   NAck extends SendStatus

      sealed trait  HttpInteract[A]
      case object   HttpReceive extends HttpInteract[RecvError \/ HttpReq]
      case class    HttpRespond(data: HttpResp) extends HttpInteract[SendStatus]
      case class    Stop(error: RecvError \/ SendStatus) extends HttpInteract[RecvError \/ SendStatus]

      sealed trait  HttpHandle[A]
      case class    HttpHandleResult(resp: HttpResp) extends HttpHandle[HttpResp]

      // APP DEFINITION
      type App[A] = HttpInteract[A] :+: HttpHandle[A] :+: Log[A] :+: DB.FreeApp[A] :+: CNil
      type CoyoApp[A] = Coyoneda[App, A]
      type FreeApp[A] = Free.FreeC[App, A]


      // HELPERS
      def lift[F[_], A](a: F[A])(implicit inj: Inject[App[A], F[A]]): FreeApp[A] = Copoyo[App](a)

      object HttpInteract {
        def receive() = lift(HttpReceive)
        def respond(data: HttpResp) = lift(HttpRespond(data))
        def stop(err: RecvError \/ SendStatus) = lift(Stop(err))
      }

      object Log {
        def info(msg: String)       = lift(LogMsg(InfoLevel, msg))
      }

      object HttpHandle {
        def result(resp: HttpResp) = lift(HttpHandleResult(resp))
      }

      // Handle action
      def handle(req: HttpReq): FreeApp[HttpResp] = req.url match {
        case "/foo" =>
          for {
            dbRes <-  lift(DB.findById("foo"))

            resp  <-  HttpHandle.result(
                        dbRes match {
                          case -\/(err) => HttpResp(status = InternalServerError)
                          case \/-(e)   => HttpResp(status = Ok, body = e.toString)
                        }
                      ) 
          } yield (resp)

        case _ => HttpHandle.result(HttpResp(status = InternalServerError))
      }

      // Server
      def serve(): FreeApp[RecvError \/ SendStatus] =
        for {
          recv  <-  HttpInteract.receive()
          _     <-  Log.info("HttpReceived Request:"+recv)
          res   <-  recv match {
                      case -\/(err) => HttpInteract.stop(-\/(err))

                      case \/-(req) => for {
                        resp  <- handle(req)
                        _     <- Log.info("Sending Response:"+resp)
                        ack   <- HttpInteract.respond(resp)
                        res   <- if(ack == Ack) serve() else HttpInteract.stop(\/-(ack))
                      } yield (res)
                    }
        } yield (res)

    }


    //////////////////////////////////////////////////////////////////////////
    // Compile Languages

    /////////////////////////////////////////////////////////////////
    // Pure
    object Logger extends (Log ~> Id) {
      def apply[A](a: Log[A]) = a match {
        case LogMsg(lvl, msg) =>
          println(s"$lvl $msg")
      }
    }

    object DBManager extends (DB.DBInteract ~> Id) {
      def apply[A](a: DB.DBInteract[A]) = a match {
        case DB.FindById(id) =>
          println(s"DB Finding $id")
          \/-(Map("id" -> id, "name" -> "toto"))
      }
    }

    object HttpHandler extends (Http.HttpHandle ~> Id) {
      def apply[A](a: Http.HttpHandle[A]) = a match {
        case Http.HttpHandleResult(resp) =>
          println(s"Handling $resp")
          resp
      }
    }

    object HttpInteraction extends (Http.HttpInteract ~> Id) {
      var i = 0
      def apply[A](a: Http.HttpInteract[A]) = a match {
        case Http.HttpReceive       => 
          if(i < 1000) {
            i+=1
            \/-(Http.GetReq("/foo"))
          } else {
            -\/(Http.ClientDisconnected)
          }

        case Http.HttpRespond(resp) => Http.Ack

        case Http.Stop(err) => err
      }
    }

    object Trampolined extends (Id ~> Trampoline) {
      def apply[A](a: Id[A]) = Trampoline.done(a)
    }

    val dbInterpreter: DB.App ~> Id = DBManager ||: Logger
    val dbInterpreterCoyo: DB.CoyoApp ~> Id = liftCoyoLeft(dbInterpreter)
    val dbInterpreterFree: DB.FreeApp ~> Id = liftFree(dbInterpreterCoyo)
    
    val httpInterpreter: Http.App ~> Id = HttpInteraction ||: HttpHandler ||: Logger ||: dbInterpreterFree
    val httpInterpreterCoyo: Http.CoyoApp ~> Id = liftCoyoLeft(httpInterpreter)

    Http.serve().foldMap(Trampolined compose httpInterpreterCoyo).run

  }
}



/*


sealed trait SendStatus
case object  Ack extends SendStatus
case object  NAck extends SendStatus
 
sealed trait HttpInteract[A]
case object  HttpReceive extends HttpInteract[HttpReq]
case class   HttpRespond(data: HttpResp) extends HttpInteract[SendStatus]
case object  Stop extends HttpInteract[Unit]

sealed trait  HttpHandle[A]
case class    HttpHandleResp(resp: HttpResp) extends HttpHandle[HttpResp]

trait HttpInteract {
  def receive(): HttpReq
  def respond(resp: HttpResp): SendStatus
  def stop()
}

trait HttpHandle {
  def handle(req: HttpReq): HttpResp
}

trait App extends HttpInteract with HttpHandle {
  def main() = {
    val req = receive()
    val resp = handle(req)
    val status = respond(resp)
    if(status == Ack) main() else stop()
  }
}

objet App extends App with HttpInteractImpl with HttpHandleImpl


  --->receive
  |     |
  |     |req
  |     |
  |     v
  |   handle
  |     |
  |     |resp
  |     |
  |     v
  |   respond
  |     |
  |     |
  ------=-------> Stop
    Ack    NAck 

val serve(): App[SendStatus] = for {
  req         <- receive()
  resp        <- handle(req)
  status      <- respond(resp)
  lastStatus  <- if(status == Ack) serve() else stop(status)
} yield (lastStatus)


sealed abstract class Free[F[_], A]
case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](a: F[Free[F, A]]) extends Free[F, A]


def point[A](a: => A): Free[F, A] = Return(a)

def flatMap[B](f: A => Free[F, B])(implicit F: Functor[F]): Free[F, B] = this match {
  case Return(a)  => f(a)
  case Suspend(s) => Suspend(F.map(s){ free => free flatMap f })
}


val serve(): App[SendStatus] = for {
  req         <- liftF(HttpReceive[HttpReq]())
  resp        <- liftF(HttpHandle[HttpResp](req))
  status      <- liftF(HttpRespond[SendStatus](resp))
  lastStatus  <- if(status == Ack) liftF(serve())
else liftF(HttpStop[SendStatus](status))
} yield (lastStatus)



Suspend[ F[Suspend[ F[Suspend[ F[Return[A]] ] ] ] ] ]



type F[A] = ()
Return(a)
Suspend(())

type Option[A] = Free[[x] => (), A]

type F[E, A] = (E, A)
Suspend( (1, Suspend( (2, Suspend( (3, Return(4)) ) ) ) ) )

type Source[E] = Free[[x] => (E, x), A]

type F[A] = () => A
Suspend(() => Suspend(() => Suspend(() => Return(a))))

type Trampoline[A] = Free[[x] => (() => x), A]


type F[I, A] = I => A
Suspend(i => Suspend(i => Suspend(i => Return(4)))))

type Iteratee[I, O] = Free[[x] => (I => x), O]


trait CoYoneda[F[_], A] {
  type I
  def f: I => A
  def fi: F[I]
}

def toCoYo[F[_],A](fa: F[A]) = new CoYoneda[F,A] {
  type I = A
  val f = (a: A) => a
  val fi = fa
}

def fromCoYo[F[_]:Functor,A](yo: CoYo) = Functor[F].map(yo.fi)(yo.f)

/** `F[A]` converts to `Coyoneda[F,A]` for any `F` */
def lift[F[_],A](fa: F[A]): Coyoneda[F, A] = apply(fa)(identity[A])

/** `Coyoneda[F,_]` is a functor for any `F` */
implicit def coyonedaFunctor[F[_]]: Functor[({type λ[α] = Coyoneda[F, α]})#λ] =
  new Functor[({type λ[α] = Coyoneda[F,α]})#λ] {
    def map[A, B](ya: Coyoneda[F, A])(f: A => B) = new Coyoneda[F, B] {
      type I = ya.I
      val f = f compose ya.f // I => A => B == I => B
      val fi = ya.fi
    }
  }

  /** A free monad over a free functor of `F`. */
  def liftFC[F[_], A](fa: F[A]):  Free[[x] => Coyoneda[F, x], A] =
    liftF(Coyoneda.toCoyo(fa)

  type FreeC[S[_], A] = Free[[x] => Coyoneda[S, x], A]

  val serve(): App[SendStatus] = for {
    req         <- FreeC[HttpInteract, HttpReq]
    _           <- FreeC[Log, Unit]
    resp        <- FreeC[HttpHandle, HttpResp]
    status      <- FreeC[HttpInteract, SendStatus]
    lastStatus  <- if(status == Ack) App[SendStatus]
                   else FreeC[HttpInteract, SendStatus]
  } yield (lastStatus)


  App[A] = FreeC[HttpInteract, A] or FreeC[Log, A] or FreeC[HttpHandle, A]

  A : HttpReq => Unit => HttpResp => SendStatus

  type App[A] = FreeC[HttpInteract :+: Log :+: HttpHandle :+: CNil, A]


  object Coproduct {
    ...
    def apply[C <: Coproduct, T](implicit inj: Inject[C, T]): C = inj(t)
    ...
  }

  trait Inject[C <: Coproduct, I] {
    def apply(i: I): C
  }

  val c: App[HttpReceive[HttpReq]] = Coproduct[App](HttpReceive[HttpReq]())




// APP DEFINITION
type App[A] = HttpInteract[A] :+: HttpHandle[A] :+: Log[A] :+: CNil
type CoyoApp[A] = Coyoneda[App, A]
type FreeApp[A] = Free.FreeC[App, A]

// HELPERS
def lift[F[_], A](a: F[A])(implicit inj: Inject[App[A], F[A]]): FreeApp[A] = Free.liftFC(Coproduct[App[A]](fa))

object HttpInteract {
  def receive() = lift(HttpReceive)
  def respond(data: HttpResp) = lift(HttpRespond(data))
  def stop(err: RecvError \/ SendStatus) = lift(Stop(err))
}

object Log {
  def info(msg: String)       = lift(LogMsg(InfoLevel, msg))
}

object HttpHandle {
  def result(resp: HttpResp) = lift(HttpHandleResult(resp))
}



// APP DEFINITION
type App[A] = HttpInteract[A] :+: HttpHandle[A] :+: Log[A] :+: CNil
type CoyoApp[A] = Coyoneda[App, A]
type FreeApp[A] = Free.FreeC[App, A]

val serve(): FreeApp[SendStatus] = for {
  req         <- receive()
  _           <- log("Received: "+req.toString)
  resp        <- handle(req)
  status      <- respond(resp)
  lastStatus  <- if(status == Ack) serve() else stop(status)
} yield (lastStatus)

// Handle action
def handle(req: HttpReq): FreeApp[HttpResp] = req.url match {
  case "/foo" =>
    for {
      dbRes <-  lift(DB.findById("foo"))

      resp  <-  HttpHandle.result(
                  dbRes match {
                    case -\/(err) => HttpResp(status = InternalServerError)
                    case \/-(e)   => HttpResp(status = Ok, body = e.toString)
                  }
                ) 
    } yield (resp)

  case _ => HttpHandle.result(HttpResp(status = InternalServerError))
}

sealed trait Free[F[_], A]
case class Return[F[_], A](a: A) extends Free[F, A]
case class Bind[F[_], A](
  i: F[I],
  f: I => Free[F, A]
) extends Free[F, A]

sealed trait List[A]
case class Nil extends List[Nothing]
case class Cons(h
  ead: A,
  tail: List[A]
) extends List[A]

/**
 * Catamorphism for `Free`.
 * Runs to completion, mapping the suspension with the given transformation at each step and
 * accumulating into the monad `M`.
 */
def foldMap[M[_]](f: S ~> M)(implicit S: Functor[S], M: Monad[M]): M[A] =
  this match {
    case Suspend(s) => Monad[M].bind(f(s))(_.foldMap(f))
    case Return(r) => Monad[M].pure(r)
  }

/** Changes the suspension functor by the given natural transformation. */
def mapSuspension[T[_]](f: S ~> T)(implicit S: Functor[S], T: Functor[T]): Free[T, A] =
  this match {
    case Suspend(s) => Suspend(f(S.map(s)(((_: Free[S, A]) mapSuspension f))))
    case Return(r) => Return(r)
  }


val fi: (a:A) => Free[F, A] = ???
val free: Free[F, A] = Return(a) >>= f1 >>= f2 >>= f3 >>= f4



HttpInteract ~> F
HttpHandle ~> F
Log ~> F

HttpInteract :+: HttpHandle :+: Log :+: CNil ~> F

object Logger extends (Log ~> Id) {
  def apply[A](a: Log[A]) = a match {
    case LogMsg(lvl, msg) =>
      println(s"$lvl $msg")
  }
}

object HttpHandler extends (Http.HttpHandle ~> Id) {
  def apply[A](a: Http.HttpHandle[A]) = a match {
    case Http.HttpHandleResult(resp) =>
      println(s"Handling $resp")
      resp
  }
}

object HttpInteraction extends (Http.HttpInteract ~> Id) {
  def apply[A](a: Http.HttpInteract[A]) = a match {
    case Http.HttpReceive       => Http.GetReq("/foo")

    case Http.HttpRespond(resp) => Http.Ack

    case Http.Stop(err) => err
  }
}



(((a1 ++ a2) ++ a3) ++ .. ++ an))) => a1 ++ (a2 ++ (a3 ++ (.. ++ an)))
(((a1 >>= a2) >>= a3) >>= .. >>= an))) => a1 >>= (a2 >>= (a3 >>= (.. >>== an)))
*/


