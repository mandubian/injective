import org.scalatest._

import scalaz.Monad
import annotation.tailrec

import scalaz._
import Scalaz._

class BasicFreeSpec extends FlatSpec with Matchers with Instrumented {


  "Scalaz Free" should "left/right bind" in {
    import Free._

    val M = implicitly[Monad[Trampoline]]

    def gen[I](i: I): Trampoline[I] = {
      Suspend( () => Trampoline.done(i) )
    }

    //(a flatMap (b flatMap (c flatMap (...))))
    def lftBind(n: Int) = {
      (1 to n).foldLeft(gen(0)){ case (acc, i) => acc flatMap { a => gen(i) } }
      // @tailrec def step(i: Int, free: Trampoline[Int]): Trampoline[Int] = {
      //   if(i < n) step(i+1, free flatMap { a => gen(i) })
      //   else free
      // }

      // step(0, gen(0))
    }

    // (... flatMap (_ => c flatMap (_ => b flatMap (_ => a))))
    def rgtBind(n: Int) = {
      (1 to n).foldRight(gen(n)){ case (i, acc) => gen(i) flatMap { a => acc } }
      // @tailrec def step(i: Int, free: Trampoline[Int]): Trampoline[Int] = {
      //   if(i > 0) step(i-1, gen(i) flatMap { _ => free })
      //   else free
      // }

      // step(n, gen(n))
    }

    val testN = Seq[Int](
      // 1000,
      // 100000, 200000, 300000, 500000, 800000,
      // 1000000, 2000000, 3000000, 5000000,
      // 10000000, 15000000
      // 15000000
    )

    testN foreach { n =>
      testTime(s"Scalaz Free - Left Bind - $n") { lftBind(n).run } should equal (n)
      // testTime(s"Scalaz Free - Right Bind - $n") { rgtBind(n).run } should equal (n)
    }
    // testTime("Scalaz Free - Left  Bind - 1000")     { lftBind(1000).run } should equal     (1000)
    // testTime("Scalaz Free - Left  Bind - 100000")   { lftBind(100000).run } should equal   (100000)
    // testTime("Scalaz Free - Left  Bind - 1000000")  { lftBind(1000000).run } should equal  (1000000)
    // testTime("Scalaz Free - Left  Bind - 10000000") { lftBind(10000000).run } should equal (10000000)

    // testTime("Scalaz Free - Right Bind - 1000")     { rgtBind(1000).run } should equal      (1000)
    // testTime("Scalaz Free - Right Bind - 100000")   { rgtBind(100000).run } should equal   (100000)
    // testTime("Scalaz Free - Right Bind - 1000000")  { rgtBind(1000000).run } should equal  (1000000)
    // testTime("Scalaz Free - Right Bind - 5000000") { rgtBind(5000000).run } should equal (5000000)

  }


  "Fixed Free" should "left/right bind" in {
  	import TFree._
    import TFreeView._

    //val M = TFreeMonad[Function0]

    def gen[I](i: I): Trampoline[I] = {
      fromView(Impure[Function0, I]( () => Trampoline.done(i) ))
    }

    //(a flatMap (b flatMap (c flatMap (...))))
    def lftBind(n: Int) = {
      // (1 to n).foldLeft(gen(0)){ case (acc, i) => acc flatMap { a => gen(i) } }
      @tailrec def step(i: Int, free: Trampoline[Int]): Trampoline[Int] = {
        if(i <= n) step(i+1, free flatMap { a => gen(i) })
        else free
      }

      step(0, gen(0))
    }

    // (... flatMap (_ => c flatMap (_ => b flatMap (_ => a))))
    def rgtBind(n: Int) = {
      (1 to n).foldRight(gen(n)){ case (i, acc) => gen(i) flatMap { a => acc } }
      // @tailrec def step(i: Int, free: Trampoline[Int]): Trampoline[Int] = {
      //   if(i > 0) step(i-1, gen(i) flatMap { _ => free })
      //   else free
      // }

      // step(n, gen(n))
    }

    val testN = Seq[Int](
      1000,
      // 100000, 200000, 300000, 500000, 800000,
      // 1000000, 2000000, 3000000, 5000000,
      // 10000000, 15000000
      19000000
    )

    testN foreach { n =>
      testTime(s"Fixed Free - Left Bind  - $n") { lftBind(n).run } should equal (n)
      // testTime(s"Fixed Free - Right Bind - $n") { rgtBind(n).toView } //should equal (n)
    }

    // testTime("Fixed Free - Left  Bind - 1000")     { lftBind(1000).run } should equal     (1000)
    // testTime("Fixed Free - Left  Bind - 100000")   { lftBind(100000).run } should equal   (100000)
    // testTime("Fixed Free - Left  Bind - 1000000")  { lftBind(1000000).run } should equal  (1000000)
    // testTime("Fixed Free - Left  Bind - 10000000") { lftBind(10000000).run } should equal (10000000)

    // testTime("Fixed Free - Right Bind - 1000")     { rgtBind(1000).run } should equal      (1000)
    // testTime("Fixed Free - Right Bind - 100000")   { rgtBind(100000).run } should equal   (100000)
    // testTime("Fixed Free - Right Bind - 1000000")  { rgtBind(1000000).run } should equal  (1000000)
    // testTime("Fixed Free - Right Bind - 10000000") { rgtBind(10000000).run } should equal (10000000)

  }

}
