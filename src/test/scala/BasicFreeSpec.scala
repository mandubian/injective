import org.scalatest._

import scalaz.Monad

class BasicFreeSpec extends FlatSpec with Matchers with Instrumented {

  "TFree" should "left bind" in {
  	import TFree._

  	val M = implicitly[Monad[({ type l[A] = Source[Int, A] })#l]]

  	def doit(n: Int) = (1 to n).foldLeft(M.point(0)){ case (acc, i) => acc flatMap { a => M.point(i) } }

  	println("doit:"+doit(10))
  }

}
