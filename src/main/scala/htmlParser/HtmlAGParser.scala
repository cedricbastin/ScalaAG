package htmlParser

import agParsers.AGParsers //helper functions for TypingResults
import agParsers.AGSig

/**
* Created by cedricbastin on 25/04/15.
*/
/*
| is the alternation combinator. It says “succeed if either the left or right operand parse successfully”
~ is the sequential combinator. It says “succeed if the left operand parses successfully, and then the right parses successfully on the remaining input”
~> says “succeed if the left operand parses successfully followed by the right, but do not include the left content in the result”
<~ is the reverse, “succeed if the left operand is parsed successfully followed by the right, but do not include the right content in the result”
^^=> is the transformation combinator. It says “if the left operand parses successfully, transform the result using the function on the right”
rep => simply says “expect N-many repetitions of parser X” where X is the parser passed as an argument to rep
*/

trait HtmlSig extends AGSig {
  def start(tag:String, a:Answer):Answer
  def end(tag:String, a:Answer):(Boolean, Answer)
  //def add(tag:String, a:Answer):Answer
}

case class HtmlContainer(tag:String, content:List[HtmlContainer]) {
  def print:Unit = {
    println("<" + tag + ">\r\n")
    content.foreach{_.print}
    println("\r\n<\\" + tag + ">\r\n")
  }
}

trait HtmlGrammar extends AGParsers with HtmlSig {
  lexical.delimiters ++= List("<", ">", "\\")
  //lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ", "pred", "iszero", "let", "in")

  def Start: AGParser[String] = {
    //(f: T => U, add:(Answer, U) => Answer)
    lift(keyword("<")) ~> lift(ident) <~ lift(keyword(">")) >>^^>> {
      case (tag:String, a:Answer) => (start(tag, a), tag)
    }
  }

  def End: AGParser[String] = {
    lift("<") ~> lift("\\") ~> lift(ident) <~ lift(">") >>^^>> {
      case (tag:String, ans:Answer) =>
        val res = end(tag, ans)
//        if (!res._1)
//          null
//        else
          (res._2, tag)
    }
  }

//  def Container2: AGParser[HtmlContainer] = {
//    Container ~ Container ^^ { //there should always be a big outer container: e.g. body
//      case c1 ~ c2 => HtmlContainer("hello", c1 :: c2 :: Nil)
//    } | lift(failure("illegal start of container"))
//  }

  def Container: AGParser[HtmlContainer] = {
     Start >>~>> rep(Container) >>~>> End ^^ { //there should always be a big outer container: e.g. body
      case start ~ cont ~ end => HtmlContainer(start, cont)
    } | lift(failure("illegal start of container"))
  }

}

trait HtmlAlgebra extends HtmlSig {
  type Answer = List[String] //stack where we push and pop the head

  def start(tag:String, a:Answer) = tag :: a //return answer??
  def end(tag:String, a:Answer) = a match {
      case x :: xs if x == tag => (true, xs)
      case _ => (false, a)
    }
  //def add(tag:String, a:Answer):Answer = tag :: a
  def combine(a1:Answer, a2: Answer) = a1 ::: a2
}

class HtmlTest extends HtmlGrammar with HtmlAlgebra {
    def test(in:String) = {
      print(in+"=> ")
      val tokens = new lexical.Scanner(in)
      val initAns = Nil
      val parsed = Container(initAns, tokens)
      parsed match {
        case AGSuccess(res, next, ans) =>
          println(res)
        case e =>
          println("parsing failure: "+e)
      }
    }
    def testAll() = {
      test("<test><\\test>")
      test("<test><c1><\\c1><\\test>")
      test("<test><\\test><c1><\\c1>")
      test("<test><c1><c2><\\c2><\\c1><\\test>")
      test("<test><c1><\\c1><c2><\\c2><\\test>")
      test("<test><c1><\\c1><c2><\\c3><\\test>")
    }
}

object Test extends App {
  new HtmlTest().testAll()
}
