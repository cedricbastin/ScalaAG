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
  def defAttr:Attr //default attribute
  def start(tag:String):(Attr, AttrEnv)
  def end(tag:String, a:AttrEnv):(Attr, AttrEnv)
  def validate(tag:String, a:AttrEnv):Boolean
  def validateFull(syn:List[Attr], in:AttrEnv):Boolean
  def collect(a:AttrEnv):(Attr, AttrEnv)
}

case class Container(tag:String, body:List[Container]) {
  def print:Unit = print("")
  def print(s:String):Unit = {
    println(s+"<" + tag +">")
    body.reverse.foreach(_.print(s+"  "))
    println(s+"<\\" + tag + ">")
  }
}

trait HtmlGrammar extends AGParsers with HtmlSig {
  lexical.delimiters ++= List("<", ">", "\\", """"""", "=")
  lexical.reserved ++= List("x")

  def HtmlP:AGParser[List[Attr]] = {
    (repWithAns(Collect | End(ident) | Start(ident)) validate {
      case (list, inAttrs) => validateFull(list, inAttrs)
    }) >>^^>> { (x, ans) => (x, ans) }
  }

  def Start(pars:Parser[String]): AGParser[Attr] = {
    lift(keyword("<")) ~> lift(pars) <~ lift(keyword(">")) ^^>> {
      case tag => start(tag)
    }
  }

  def End(pars:Parser[String]): AGParser[Attr] = {
    (lift("<") ~> lift("\\") ~> lift(pars) <~ lift(">") validate {
      case (s, ans) =>
        validate(s, ans)
    }) >>^^>> {
      case (tag, ans) => end(tag, ans)
    }
  }

  def Collect: AGParser[Attr] = {
    Start("x") >>~>> Start(ident) >>~>> End(ident) >>~>> End("x") >>^^>> {
      case (startX ~ start ~ end ~ endX, ans) => collect(ans)
    }
  }
}

trait HtmlAlgebra extends HtmlSig {
  type Attr = Option[Container] //we build a recognizer rather than a parser
  type AttrEnv = List[Either[String, Container]] //stack where we push and pop the head
  val defAttr = None //the collector doesn't collect anything by default

  def start(tag:String):(Attr, AttrEnv) = (defAttr, Left(tag) :: Nil) //push stuff to head

  def end(tag:String, a:AttrEnv):(Attr, AttrEnv) = {
    val (conts, tags) = a span {case Right(_) => true case _ => false} //collect all containers
    tags match {
      case Left(s) :: xs =>
        assert(s == tag) //validation should have been done beforehand
        val nx:Either[String, Container] = Right(Container(tag, conts.collect{ case Right(c) => c}))
        (defAttr, nx :: xs)
      case _ => //should never happens
        assert(false)
        null
    }
  }

  //collect the last see container, you should know what it is
  def collect(a:AttrEnv):(Attr, AttrEnv) = a match {
    case Right(x) :: xs => (Some(x), a)
    case _ => assert(false); (None, a) //
  }

  //validate that the string value of a closing tag corresponds to the last opening tag
  def validate(tag:String, a:AttrEnv) = {
    a.find{case Left(_) => true case _ => false} match { //last added opening tag
      case Some(Left(t)) => tag == t //matching open and closing tags
      case _ => false
    }
  }

  //validate that a fully parsed xml has all it's containers closed
  def validateFull(syn:List[Attr], in:AttrEnv):Boolean = {
    in.forall{case Right(_) => true case _ => false} //no unclosed container left
  }

  def combine(old:AttrEnv, niew: AttrEnv) = niew ::: old //TODO: is this a good idea?, when to use it?
}

class HtmlTest extends HtmlGrammar with HtmlAlgebra {
  def test(in:String) = {
    println("input: "+in+" => ")
    val tokens:lexical.Scanner = new lexical.Scanner(in)
    val initAns = Nil
    val parsed = HtmlP(initAns, tokens)
    parsed match {
      case AGSuccess(res, next, ans) =>
        println("validated: ")
        ans.reverse.foreach{case Right(c) => c.print case _ => ()}
        println("")
        println("collected: ")
        res.collect{case Some(c) => c}.reverse.foreach(_.print)
      case e =>
        println("parsing failure: "+e)
    }
  }
  def testAll() = {
    test("<a> <\\a>")
    test("<a> <\\aX>")
    test("<x> <c> <\\c> <\\x>")
    test("<a> <b> <\\bx> <\\a>")
    test("<x> <\\x>")
    test("<a> <x> <c> <\\c> <\\x> <b> <x> <a> <\\a> <\\x> <\\b> <\\a> <x> <d> <\\d> <\\x>")
  }
}

object Test extends App {
  new HtmlTest().testAll()
}
