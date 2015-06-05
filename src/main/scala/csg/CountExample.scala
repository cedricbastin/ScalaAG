package csg

import agParsers._

/**
 * Created by cedricbastin on 02/05/15.
 */

trait CountSig extends AGSig {
  trait InAttrs
  case class Env(max:Int, count:Int, c:Char) extends InAttrs
  case object Fail extends InAttrs
  def combine(a1:InAttrs, a2:InAttrs) = {
    a1 //useless?
  }
  //def next(a:Answer): Answer
}

trait CountGrammar extends AGParsers with CountSig {
  //lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+")
  lexical.reserved ++= List("a", "b", "c")

  def Next: AGParser[InAttrs] = {
    rep(lift("a")) ~>> lift("b") ~>> lift("c") ^^ {
      case a ~ b ~ c => Env(1,2,'c')
    }
  }
}

trait CountAlgebra extends CountSig {

}

class CountExample extends CountGrammar with CountAlgebra {

}
