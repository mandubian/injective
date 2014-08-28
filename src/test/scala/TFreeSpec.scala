/**
  * Copyright 2014 Pascal Voitot (@mandubian)
  *
  * But deeply inspired by Scala Async project <https://github.com/scala/async>
  */
import org.scalatest._

import scalaz._
import Scalaz._

import scala.annotation.tailrec

object TFreeTest {
  import TFree._
  import TFreeView._

  case class Get[I, A](f: I => A)

  object Get {
    implicit def F[I] = new Functor[({ type l[T] = Get[I, T]})#l] {
      def map[A, B](a: Get[I, A])(f: A => B): Get[I, B] = Get[I, B](i => f(a.f(i)))
    }
  }

  type It[I, A] = TFree[({ type l[T] = Get[I, T]})#l, A]

  object It {
    implicit def Monad[S[_], I]: Monad[({ type l[X] = It[I, X] })#l] = TFreeMonad[({ type l[T] = Get[I, T]})#l]
  }

  def get[I](implicit M: Monad[({ type l[X] = It[I, X] })#l]) : It[I, I] = {
    import It._

    type Get0[T] = ({ type l[T] = Get[I, T]})#l[T]

    fromView[Get0, I](
      Impure[Get0, I](
        Get( (i:I) => M.point(i) )
      )
    )
  }

  def addGet(i: Int)(implicit M: Monad[({ type l[X] = It[Int, X] })#l]): It[Int, Int] = {
    get[Int] map { x => x + i }
  }

  def addNbad(n: Int)(implicit M: Monad[({ type l[X] = It[Int, X] })#l]): It[Int, Int] = {
    Seq.fill(n)(addGet _).foldLeft(M.point[Int](0)){ case (acc, f) => M.bind(acc)(f) }
  }

  def feedAll[I, A](it: It[I, A])(l: Seq[I])(implicit M: Monad[({ type l[X] = It[I, X] })#l]): Option[A] = {
    @tailrec def step(it: It[I, A])(l: Seq[I]): Option[A] = {
      it.toView match {
        case t: Pure[({ type l[T] = Get[I, T]})#l, a]   => Some(t.a)
        case t: Impure[({ type l[T] = Get[I, T]})#l, a] =>
          l match {
            case Nil    => None
            case h +: l => step(t.a.f(h))(l)
          }
      }
    }

    step(it)(l)
  }
}


object FreeTest {
  import Free._
  import FreeView._

  case class Get[I, A](f: I => A)

  sealed abstract class FreeView[S[_], A] 

  object FreeView {
    case class Pure[S[_], A](a: A) extends FreeView[S, A]
    case class Impure[S[_], A](a: S[Free[S, A]]) extends FreeView[S, A]
  }


  def toView[S[_]: Functor, A](free: Free[S, A]): FreeView[S, A] = free.resume match {
    case \/-(a) => Pure(a)
    case -\/(s) => Impure(s)
  }

  def fromView[S[_], A](v: FreeView[S, A]): Free[S, A] = v match {
    case Pure(a)    => Free.Return(a)
    case Impure(f)  => Free.Suspend(f)
  }

  object Get {
    implicit def F[I] = new Functor[({ type l[T] = Get[I, T]})#l] {
      def map[A, B](a: Get[I, A])(f: A => B): Get[I, B] = Get[I, B](i => f(a.f(i)))
    }
  }

  type It[I, A] = Free[({ type l[T] = Get[I, T]})#l, A]

  object It {
    implicit def Monad[S[_], I]: Monad[({ type l[X] = It[I, X] })#l] = freeMonad[({ type l[T] = Get[I, T]})#l]
  }

  def get[I](implicit M: Monad[({ type l[X] = It[I, X] })#l]) : It[I, I] = {
    import It._

    type Get0[T] = ({ type l[T] = Get[I, T]})#l[T]

    fromView[Get0, I](
      Impure[Get0, I](
        Get( (i:I) => M.point(i) )
      )
    )
  }

  def addGet(i: Int)(implicit M: Monad[({ type l[X] = It[Int, X] })#l]): It[Int, Int] = {
    get[Int].map { x => x + i }
  }

  def addNbad(n: Int)(implicit M: Monad[({ type l[X] = It[Int, X] })#l]): It[Int, Int] = {
    //Seq.fill(n)(addGet _).foldLeft(M.point[Int](0)){ case (acc, f) => M.bind(acc)(f) }

    @scala.annotation.tailrec
    def step(i: Int, it: It[Int, Int]): It[Int, Int] = {
      if(i < n) step(i+1, M.bind(it)(addGet _)) else it
    }

    step(0, M.point[Int](0))
  }

  def feedAll[I, A](it: It[I, A])(l: Seq[I])(implicit M: Monad[({ type l[X] = It[I, X] })#l]): Option[A] = {
    @tailrec def step(it: It[I, A])(l: Seq[I]): Option[A] = {
      toView[({ type l[T] = Get[I, T]})#l, A](it) match {
        case t: Pure[({ type l[T] = Get[I, T]})#l, a]   => Some(t.a)
        case t: Impure[({ type l[T] = Get[I, T]})#l, a] =>
          l match {
            case Nil    => None
            case h +: l => step(t.a.f(h))(l)
          }
      }
    }

    step(it)(l)
  }
}


class TFreeSpec extends FlatSpec with Matchers with Instrumented {


  "TFree" should "compile" in {
    import TFreeTest._
    import It._

    def testQuadratic(n: Int) = feedAll(addNbad(n))(1 to n)

    testTime("TFree 10000") { println(testQuadratic(10000)) }
    testTime("TFree 20000") { println(testQuadratic(20000)) }
    testTime("TFree 30000") { println(testQuadratic(30000)) }
    testTime("TFree 40000") { println(testQuadratic(40000)) }
    testTime("TFree 50000") { println(testQuadratic(50000)) }
    testTime("TFree 100000") { println(testQuadratic(100000)) }
    testTime("TFree 200000") { println(testQuadratic(200000)) }
    testTime("TFree 300000") { println(testQuadratic(300000)) }
    testTime("TFree 400000") { println(testQuadratic(400000)) }
    testTime("TFree 500000") { println(testQuadratic(500000)) }
    testTime("TFree 1000000") { println(testQuadratic(1000000)) }
    testTime("TFree 2000000") { println(testQuadratic(2000000)) }
    testTime("TFree 3000000") { println(testQuadratic(3000000)) }
    testTime("TFree 4000000") { println(testQuadratic(4000000)) }
    testTime("TFree 5000000") { println(testQuadratic(5000000)) }
    testTime("TFree 10000000") { println(testQuadratic(10000000)) }

  }

  // "Free" should "compile" in {
  //   import FreeTest._
  //   import It._

  //   def testQuadratic(n: Int) = feedAll(addNbad(n))(1 to n)

  //   testTime("Free 10000") { println(testQuadratic(10000)) }
  //   testTime("Free 20000") { println(testQuadratic(20000)) }
  //   testTime("Free 30000") { println(testQuadratic(30000)) }
  //   testTime("Free 40000") { println(testQuadratic(40000)) }
  //   testTime("Free 50000") { println(testQuadratic(50000)) }
  //   testTime("Free 100000") { println(testQuadratic(100000)) }
  //   testTime("Free 200000") { println(testQuadratic(200000)) }
  //   testTime("Free 300000") { println(testQuadratic(300000)) }
  //   testTime("Free 400000") { println(testQuadratic(400000)) }
  //   testTime("Free 500000") { println(testQuadratic(500000)) }
  //   testTime("Free 1000000") { println(testQuadratic(1000000)) }
  //   testTime("Free 2000000") { println(testQuadratic(2000000)) }
  //   testTime("Free 3000000") { println(testQuadratic(3000000)) }
  //   testTime("Free 4000000") { println(testQuadratic(4000000)) }
  //   testTime("Free 5000000") { println(testQuadratic(5000000)) }
  //   testTime("Free 10000000") { println(testQuadratic(10000000)) }

  // }
}