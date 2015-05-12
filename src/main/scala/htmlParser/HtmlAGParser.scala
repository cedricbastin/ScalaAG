//package htmlParser
//
//import agParsers.AGParsers //helper functions for TypingResults
//import agParsers.AGSig
//
///**
// * Created by cedricbastin on 25/04/15.
// */
///*
//| is the alternation combinator. It says “succeed if either the left or right operand parse successfully”
//~ is the sequential combinator. It says “succeed if the left operand parses successfully, and then the right parses successfully on the remaining input”
//~> says “succeed if the left operand parses successfully followed by the right, but do not include the left content in the result”
//<~ is the reverse, “succeed if the left operand is parsed successfully followed by the right, but do not include the right content in the result”
//^^=> is the transformation combinator. It says “if the left operand parses successfully, transform the result using the function on the right”
//rep => simply says “expect N-many repetitions of parser X” where X is the parser passed as an argument to rep
// */
//
//trait HtmlSig extends AGSig {
//  def start(tag:String): Answer
//  def end(tag:String):Answer
//  def add(tag:String, a:Answer):Answer
//}
//
//case class Container(tag:String, content:Option[Container])
//
//trait StlcGrammar extends AGParsers with HtmlSig {
//  lexical.delimiters ++= List("<", ">", "\\")
//  //lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ", "pred", "iszero", "let", "in")
//
//  def Start: AGParser[String] = {
//    //(f: T => U, add:(Answer, U) => Answer)
//    lift(keyword("<")) ~> lift(ident) <~ lift(keyword(">")) ^^>>[String] (
//      {case x => x}, //we do not change the string
//      {case (answer:Answer, tag:String) => add(tag, answer)}
//      )
//  }
//
//  def End: AGParser[String] = {
//    lift("<") ~> lift("\\") ~> lift(ident) <~ lift(">") >>^^>> {
//      case (s, ans) =>
//        ans match {
//          case x :: xs if (x == s) =>
//              (xs, s) //value popped
//          case xs =>
//            failure("afjkhgjhdsaf")
//        }
//    }
//  }
//
//  def Container: AGParser[Container] = {
//     Start  ~ rep(Container) ~ End ^^ {
//      case "<" ~ start ~ ">" ~ cont ~ "<" ~ "\\" ~ end ~ ">" => Container(start, cont)
//    } | lift(failure("illegal start of container"))
//  }
//
//}
//
//trait SltcTypingAlgebra extends StlcSig {
//  \\case class Answer(env: Env, tpe: Type)
//  type Answer = List[String] \\stack where we push and pop the head
//
//  def start(tag:String) = tag //
//  def add(tag:String, a:Answer):Answer = tag :: a
//
//}
//
//object HtmlTest extends HtmlGrammar with HtmlAlgebra {
//  def test(in:String, tpe:Type) = {
//    print(in+"=> ")
//    val tokens = new lexical.Scanner(in)
//    val initAns = Answer(new HashMap(), TypeUnit)
//    val parsed = Term(initAns, tokens)
//    parsed match {
//      case AGSuccess(res, next, ans) =>
//        if (res.tpe == tpe) println("ok!")
//        else println("type mismatch: expected: "+tpe+" found:"+res.tpe)
//      case e =>
//        println("parsing failure: "+e)
//    }
//  }
//  def testAll() = {
//    test("<test><\\test>", TypeBool)
//  }
//}
