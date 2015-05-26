package agStaged

import scala.virtualization.lms.common._
import stagedparsec._
import scala.reflect.{ SourceContext, RefinedManifest }
import lms._

/**
* Created by cedricbastin on 22/05/15.
*/
trait CharRepSig extends AGSig { //TODO: also Rep[]?

  type Answer = Char

  //A bit of a hack: manifest[StringReader] causes a nullPointerException
  //possibly due to a bug in Scala
  val ansManifest = manifest[Char]
}
//
//trait CharRepOpsAlgebra extends CharRepSig {
//  type Answer = Int //e.g. count as an example
//  def combine(a1:Rep[Answer], a2:Rep[Answer]) = a1 + a2
//}

trait CharRepParsers extends AGCharParsers with CharRepSig {
  def Pars: AGParser[Int] = {
    acceptIf(x => x == unit('h')) map {case c => unit(c.toString.toInt)} //test basic functionality
  }
}

trait CharRepParsersExp extends AGCharParsersExp

trait GenCharRepParsers extends ScalaGenAGCharParsers {
  val IR: CharRepParsersExp
}

trait CharRepParsersProg extends CharRepParsers {
  import AGParser._ //for phrase

  def acceptChar(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val parser = acceptIf(x => x == unit('h'))
    val defAns: Rep[Answer] = unit('h')
    phrase(parser, StringReader(in), defAns)
  }

  def mapTest(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val parser:AGParser[Char] = acceptIf(x => x == unit('h')) map { c => c}
    val defAns: Rep[Answer] = unit('h')
    phrase(parser, StringReader(in), defAns)
  }
}


object StagedTest extends App {

  (new CharRepParsersProg //StagedParsersProg
    with CharRepParsersExp
    with MyIfThenElseExpOpt //IfThenElseExpOpt
    with StructFatExpOptCommon
    with MyScalaCompile {
    self =>

    val codegen = new GenCharRepParsers with ScalaGenFatStruct with MyScalaGenIfThenElseFat {
      val IR: self.type = self
    }

    codegen.emitSource(acceptChar _, "acceptChar", new java.io.PrintWriter(System.out))
    codegen.reset
    codegen.emitSource(mapTest _, "mapTest", new java.io.PrintWriter(System.out))
    codegen.reset

    val testcAcceptIf = compile(acceptChar)
    val mapptest = compile(mapTest)

    scala.Console.println(testcAcceptIf("abc".toArray))
    scala.Console.println(mapptest("def".toArray))
    codegen.reset

  })
}

//class CharParsersSuite extends FileDiffSuite {
//
//  val prefix = "test-out/"
//
//  def testCharParsers = {
//    withOutFile(prefix + "char-parser") {
//      /**
//       * Attention: Need to mix in Fat versions of Struct as well as IfthenElse
//       * for optimisations on FatIfs and so on.
//       * Note: We are also using our own version of IfThenElseGenFat
//       * to generate variables instead of tuples and boundary ends
//       * of conditional expressions.
//       */
//      new StagedParsersProg
//        with CharParsersExp
//        with MyIfThenElseExpOpt //IfThenElseExpOpt
//        with StructFatExpOptCommon
//        with MyScalaCompile {
//        self =>
//
//        val codegen = new ScalaGenCharParsers with ScalaGenFatStruct with MyScalaGenIfThenElseFat {
//          val IR: self.type = self
//        }
//
//        codegen.emitSource(acceptIf _, "acceptIf", new java.io.PrintWriter(System.out))
//        codegen.reset
//
//      }
//      assertFileEqualsCheck(prefix + "char-parser")
//    }
//  }
//}
