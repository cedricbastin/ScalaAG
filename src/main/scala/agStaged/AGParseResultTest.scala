//package agStaged
//
//import stagedparsec._
//import scala.virtualization.lms.common._
//
//
//trait StagedSig extends AGSig {
//  def start(tag:String, a:Answer):Answer
//  //def add(tag:String, a:Answer):Answer
//}
//
//trait StagedGrammar extends AGStagedParsers with CharParsers with StagedSig {
//  //lexical.delimiters ++= List("<", ">", "\\")
//  //lexical.reserved ++= List("Hello", "Bye")
//
//  def parser(in:) = acceptIf(in, x => x == unit('h'))
//
//  def Parser: AGParser[Char] =
////    var s = Failure[Char](unit(-1))
////    val parser = accept(in, unit('h')).apply(unit(0))
////    parser{x => s = x}
////    println(s)
////  }
//
//}
//
//trait HtmlAlgebra extends HtmlSig {
//  type Answer = List[String] //stack where we push and pop the head
//
//  def start(tag:String, a:Answer) = tag :: a //return answer??
//  def end(tag:String, a:Answer) = a match {
//      case x :: xs if x == tag => (true, xs)
//      case _ => (false, a)
//    }
//  //def add(tag:String, a:Answer):Answer = tag :: a
//  def combine(a1:Answer, a2: Answer) = a1 ::: a2
//}
//
//class HtmlTest extends HtmlGrammar with HtmlAlgebra {
//  def test(in:String) = {
//    print(in+"=> ")
//    val tokens = new lexical.Scanner(in)
//    val initAns = Nil
//    val parsed = Container(initAns, tokens)
//    parsed match {
//      case AGSuccess(res, next, ans) =>
//        println(res)
//      case e =>
//        println("parsing failure: "+e)
//    }
//  }
//  def testAll() = {
//    test("<test><\\test>")
//    test("<test><c1><\\c1><\\test>")
//    test("<test><c1><\\c1><c1><\\c1><c1><\\c1><\\test>")
//    test("<test><\\test><c1><\\c1>")
//    test("<test><c1><c2><\\c2><\\c1><\\test>")
//    test("<test><c1><\\c1><c2><\\c2><\\test>")
//    test("<test><c1><\\c1><c2><\\c3><\\test>")
//  }
//}
//
//object Test extends App {
//  new HtmlTest().testAll()
//}
