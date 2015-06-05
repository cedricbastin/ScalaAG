package csg

import agParsers._

/**
 * Created by cedricbastin on 02/05/15.
 */

trait CountSig extends AGSig {
  trait AttrEnv
  case class Env(max:Int, count:Int, c:Char) extends AttrEnv
  case object Fail extends AttrEnv
  def combine(a1:AttrEnv, a2:AttrEnv) = {
    a1 //useless?
  }
  //def next(a:Answer): Answer
}

trait CountGrammar extends AGParsers with CountSig {
  //lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+")
  lexical.reserved ++= List("a", "b", "c")

  def Next: AGParser[AttrEnv] = {
    rep(lift("a")) ~>> lift("b") ~>> lift("c") ^^ {
      case a ~ b ~ c => Env(1,2,'c')
    }
  }
}

trait CountAlgebra extends CountSig {

}

class CountExample extends CountGrammar with CountAlgebra {

}
