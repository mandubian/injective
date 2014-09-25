import annotation.tailrec
import scalaz.{IndSeq, Functor, Monad, Coyoneda}

/*
sealed abstract class FreeZView[S[_], A]

object FreeZView {
  case class Pure[S[_], A](a: A) extends FreeZView[S, A]
  case class Impure[S[_], A](a: S[FreeZ[S, A]]) extends FreeZView[S, A]
}

sealed trait FreeZ[S[_], A] {

  import FreeZ._
  import FreeZView._
  import shapeless.poly.~>

  def toView(implicit F: Functor[S]): FreeZView[S, A] = FreeZ.toView(this)

  def map[B](f: A => B): FreeZ[S, B] = {
    val M = FreeZMonad[S]
    M.bind(this)( (a:A) => M.point(f(a)) )
  }

  def flatMap[B](f: A => FreeZ[S, B]): FreeZ[S, B] = {
    FreeZMonad[S].bind(this)(f)
  }

  final def mapSuspension[T[_]](f: S ~> T)(implicit S: Functor[S], T: Functor[T]): FreeZ[T, A] = {
    FreeZ.fromView(
      toView match {
        case Pure(a) => Pure[T, A](a)
        case Impure(a) => Impure[T, A](f(S.map(a){ tf => tf mapSuspension f }))
      }
    )
  }

  final def foldMap[M[_]](f: S ~> M)(implicit S: Functor[S], M: Monad[M]): M[A] = {
    toView match {
      case Pure(a)   => M.point(a)
      case Impure(a) => M.bind(f(a)){ _.foldMap(f) }
    }
  }

  final def foldMapT(f: S ~> Trampoline)(implicit S: Functor[S]): Trampoline[A] = {
    @tailrec def step(free: FreeZ[S, A]): Trampoline[A] = {
      free.toView match {
        case Pure(a)   => Trampoline.done(a)
        case Impure(a) => step(f(a).run)
      }
    }

    step(this)
  }

  /** Runs a trampoline all the way to the end, tail-recursively. */
  def run(implicit ev: FreeZ[S, A] =:= Trampoline[A]): A = {
    import scalaz.std.function._
    ev(this).go(_())
  }

  /** Runs to completion, using a function that extracts the resumption from its suspension functor. */
  final def go(f: S[FreeZ[S, A]] => FreeZ[S, A])(implicit S: Functor[S]): A = {
    @tailrec def go2(t: FreeZ[S, A]): A = {
      t.toView match {
        case Impure(a) => go2(f(a))
        case Pure(a)   => a
      }
    }
    go2(this)
  }

}

object FreeZ {
  import FreeZView._

  type FastQ[R[_, _], A, B] = IndSeq[Any]

  type FC[F[_], A, B] = A => FreeZ[F, B]
  type FMExp[F[_], A, B] = FastQ[({ type l[X, Y] = FC[F, X, Y] })#l, A, B]

  case class FM[S[_], X, A](head: FreeZView[S, X], tail: FMExp[S, X, A]) extends FreeZ[S, A]


  def fromView[S[_], A](h: FreeZView[S, A]): FreeZ[S, A] = FM(h, IndSeq[Any]())

  @tailrec def toView[S[_], A](free: FreeZ[S, A])(implicit F: Functor[S]): FreeZView[S, A] = {
    type FCS[A, B] = ({ type l[X, Y] = FC[S, X, Y] })#l[A, B]

    free match {
      case f:FM[S, x, a] => f.head match {
        case Pure(x) =>
          FastQSeq.tviewl[FCS, x, a](f.tail) match {
            case _:TViewl.EmptyL[FastQ, FCS, x] => Pure(x.asInstanceOf[A])

            case l: TViewl.LeafL[FastQ, FCS, u, v, a] =>
              toView(
                l.head(x.asInstanceOf[u]) match {
                  case f2: FM[S, x, v] =>
                    FM(f2.head, FastQSeq.tappend(f2.tail, l.tail))
                }
              )
          }
        case Impure(a) =>
          Impure(F.map(a){
            case f2: FM[S, y, x] =>
              FM(f2.head, FastQSeq.tappend(f2.tail, f.tail))
          })
      }
    }
  }

  object FastQSeq extends TSequence[FastQ] {
    def tempty[C[_, _], X]: FastQ[C, X, X] = IndSeq[Any]()

    def tsingleton[C[_, _], X, Y](c: => C[X, Y]): FastQ[C, X, Y] = IndSeq[Any](c)

    def tappend[C[_, _], X, Y, Z](a: FastQ[C, X, Y], b: => FastQ[C, Y, Z]): FastQ[C, X, Z] = {
      a ++ b
    }

    def tviewl[C[_, _], X, Y](s: FastQ[C, X, Y]): TViewl[FastQ, C, X, Y] = {
      val v = s.self.viewl
      v.headOption match {
        case None    => TViewl.EmptyL[FastQ, C, X]().asInstanceOf[TViewl[FastQ, C, X, Y]]
        case Some(h) => v.tailOption match {
          case None     => TViewl.LeafL[FastQ, C, X, Y, Y](h.asInstanceOf[C[X, Y]], IndSeq[Any]())
          case Some(t)  => TViewl.LeafL[FastQ, C, X, Any, Y](h.asInstanceOf[C[X, Any]], new IndSeq(t))
        }
      }

    }
  }

  implicit def FreeZMonad[S[_]] = new Monad[({ type l[A] = FreeZ[S, A] })#l] {

    def point[A](a: => A): FreeZ[S, A] = fromView(Pure(a))

    def bind[A, B](fa: FreeZ[S, A])(f: A => FreeZ[S, B]): FreeZ[S, B] = fa match {
      case free: FM[S, x, A] =>
        FM(
          free.head,
          FastQSeq.tappend(
            free.tail,
            FastQSeq.tsingleton(f)
          )
        )
    }
  }

  type FreeZC[S[_], A] = FreeZ[({type f[x] = Coyoneda[S, x]})#f, A]

  type Trampoline[A] = FreeZ[Function0, A]

  object Trampoline {

    def done[A](a: A): Trampoline[A] =
      FreeZ.fromView(FreeZView.Pure[Function0, A](a))

    def delay[A](a: => A): Trampoline[A] =
       suspend(done(a))

    def suspend[A](a: => Trampoline[A]): Trampoline[A] =
      FreeZ.fromView(FreeZView.Impure[Function0, A](() => a))

  }

}

*/