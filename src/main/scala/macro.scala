
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.reflect.api.Universe
import scala.reflect.internal.Flags

object FreeCompile {
  def compile[T](body: T): Unit = macro compileImpl[T]

  def compileImpl[T : c.WeakTypeTag](c: Context)(body: c.Expr[T]): c.Tree = {
    import c.universe._, c.internal._, decorators._

    val tree = body.tree
    val q"for (..$enums) yield $res" = tree

    println(s"ENUMS: $enums RES: $res")
    val code = enums map {
      case fq"$left <- Shapoyo.Copoyo.apply[$cop].apply[$f, $a]($elt)($inj)" =>
        println(s"COP=$cop F=$f A=$a elt=$elt inj=$inj")

        val ftpe = c.typecheck(
          tq"$f",
          c.TYPEmode
        ).tpe

        val atpe = c.typecheck(
          tq"$a",
          c.TYPEmode
        ).tpe

        val coptpe = c.typecheck(
          tq"$cop",
          c.TYPEmode
        ).tpe

        val tpe = appliedType(ftpe, atpe)
        val tpe2 = appliedType(coptpe, atpe)

        q"""
          val e: $tpe = $elt
          val c: $tpe2 = $inj(e)
        """
      case t =>
      c.abort(c.enclosingPosition, s"""Unmanaged tree $t""")
    }

    val block = Block(code, q"()")

    println("BLOCK:"+block)
    block
  }

}
