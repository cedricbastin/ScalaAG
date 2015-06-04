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
  def start(tag:String, ps:List[Property], a:Answer):Answer
  def end(tag:String, a:Answer):(Boolean, Answer)
  def validate(tag:String, a:Answer):Boolean
  //def add(tag:String, a:Answer):Answer
}

trait HtmlBody {
  def print
}
case class Container(tag:String, properties:List[Property], body:Body) extends HtmlBody {
  def print:Unit = {
    println("<" + tag +properties.foldLeft("")(_+_)+ ">\r\n")
    body.print
    println(" \r\n<\\" + tag + ">\r\n")
  }
}
case class Content(s:String) extends HtmlBody {
  def print:Unit = println(s)
}
case class Body(ls:List[HtmlBody]) {
  def print = ls.foreach(_.print)
}

case class Property(key:String, value:String) {
  override def toString = key+"=\""+value+"\""
}

trait HtmlGrammar extends AGParsers with HtmlSig {
  lexical.delimiters ++= List("<", ">", "\\", """"""", "=")
  //lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ", "pred", "iszero", "let", "in")

  def PropertyP: AGParser[Property] = {
    lift(ident) ~ lift("=")  ~ lift(ident) ^^ { //~ lift(""""""")
      case name ~ eq ~ value =>
        println("property")
        Property(name, value)
    }
  }

  def Start: AGParser[(String, List[Property])] = {
    //(f: T => U, add:(Answer, U) => Answer)
    lift(keyword("<")) ~> lift(ident) ~ rep(PropertyP) <~ lift(keyword(">")) >>^^>> {
      case (tag ~ ps, a:Answer) => (start(tag, ps, a), (tag, ps))
    }
  }

  def BodyP:AGParser[Body] = {
    lift(ident) ^^ { case s => Body(Content(s) :: Nil)} |
      rep(ContainerP) ^^ { case ls => Body(ls)}
  }

  def End: AGParser[String] = {
    (lift("<") ~> lift("\\") ~> lift(ident) <~ lift(">") validate {
      case (s, ans) => validate(s, ans)
    }) >>^^>> {
      case (tag, ans) =>
        val res = end(tag, ans)
        (res._2, tag)
    }
  }

  def ContainerP: AGParser[Container] = {
    Start >>~>> BodyP >>~>> End ^^ { //validate { case start ~ cont ~ end => start == end } //TODO: validation parser is not really
      case start ~ body ~ end => Container(start._1, start._2, body)
    } | lift(failure("illegal start of container"))
  }
}

trait HtmlAlgebra extends HtmlSig {
  type Answer = List[String] //stack where we push and pop the head

  def start(tag:String, ps:List[Property], a:Answer) = tag :: a //return answer??
  def end(tag:String, a:Answer) = a match {
      case x :: xs if x == tag => (true, xs)
      case _ => (false, a)
    }
  def validate(tag:String, a:Answer) = a match {
    case x :: xs if (x == tag) => true
    case _ => false
  }
  def combine(a1:Answer, a2: Answer) = a1 ::: a2
}

class HtmlTest extends HtmlGrammar with HtmlAlgebra {
    def test(in:String) = {
      print(in+"=> ")
      val tokens:lexical.Scanner = new lexical.Scanner(in)
      val initAns = Nil
      val parsed = ContainerP(initAns, tokens)
      parsed match {
        case AGSuccess(res, next, ans) =>
          println(res)
        case e =>
          println("parsing failure: "+e)
      }
    }
    def testAll() = {
      test("<test><\\testX>")
      test("""<test property =  value ><\\test>""")
      test("<test><h1><\\h2><\\test>")
      test("<test><c1><\\c1><\\test>")
      test("<test><c1><\\c1><c1><\\c1><c1><\\c1><\\test>")
      test("<test><\\test><c1><\\c1>")
      test("<test><c1><c2><\\c2><\\c1><\\test>")
      test("<test jojo=\"haha\"><c1><\\c1><c2><\\c2><\\test>")
      test("<test><c1><\\c1><c2><\\c3><\\test>")
    }
}

object Test extends App {
  new HtmlTest().testAll()
}
