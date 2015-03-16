package tqlTry

import tql._
import scala.meta.tql.ScalaMetaTraverser._
import scala.meta.internal.ast._
import scala.meta.ui._
import scala.meta.syntactic._ // q-intrpolator
import scala.language.reflectiveCalls
import scala.meta.dialects.Scala211

object IntMonoidTest extends App {
  implicit def intMonoid = new Monoid[Int] {
    def zero = 0
    def append(a: Int, b: Int) = a + b
  }

  val plus = q"""4 + 5"""

  def expli = visit({ case Lit.Int(a) => a }).bottomUp(intMonoid)
  println("expli: " + expli(plus))
  def impli = visit({ case Lit.Int(a) => a }).bottomUp //monoid is in scope
  println("impli: " + impli(plus))
  
  val test = q"""int a = 5; int b = a + a + 5"""
  println("test: " + impli(test))
}
