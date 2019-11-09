
package lintilla

import java.io.File
import language.experimental.macros
import scala.reflect.macros.Context

object Macros {

  def LINE: Int = macro lineImpl

  def lineImpl(c: Context): c.Expr[Int] = {
    import c.universe._

    val line = Literal(Constant(c.enclosingPosition.line))

    c.Expr[Int](line)
  }

  def FILE: String = macro fileImpl

  def fileImpl(c: Context): c.Expr[String] = {
    import c.universe._

    val absolute = c.enclosingPosition.source.file.file.toURI
    val base = new File(".").toURI

    val path = Literal(Constant(base.relativize(absolute).getPath))

    c.Expr[String](path)
  }
}
