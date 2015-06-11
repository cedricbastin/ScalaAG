//package csg
//
//import agParsers._
//
///**
// * Created by cedricbastin on 02/05/15.
// */
//
//trait CountSig extends AGSig {
//  def toAns(list:List[String]):Attr
//  def validate(list:List[String], env:AttrEnv):Boolean
//}
//
//trait CountGrammar extends AGParsers with CountSig {
//  //lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+")
//  lexical.reserved ++= List("a", "b", "c")
//
//  def pars = single("a") >>~>> single("b") >>~>> single("c")
//
//  def single(s:String): AGParser[Attr] = {
//    (rep(lift(s)) validate {
//      case (list, env) => validate(list, env)
//    }) ^^>> { //validateion already done!
//      case a => (a.size, )
//    }
//  }
//}
//
//trait CountAlgebra extends CountSig {
//  case class Attr(c:String, count:Int)
//  type AttrEnv = Int //count
//
//  def toAns(list:List[String]) = Attr(list.head, list.size)
//
//  def combine(a1:AttrEnv, a2:AttrEnv) = {
//    a1 //useless but required by abstract definition
//  }
//
//  def validate(list:List[String], env:AttrEnv) = {
//    (env == 0) || (list.size == env)
//  }
//}
//
//class CountExample extends CountGrammar with CountAlgebra {
//
//}
