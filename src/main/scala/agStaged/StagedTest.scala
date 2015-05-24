package agStaged

import lms._
import stagedparsec._

/**
* Created by cedricbastin on 22/05/15.
*/
//trait CharRepSig extends AGSig { //TODO: also Rep[]?
//}
//
//trait CharRepOpsAlgebra extends CharRepSig {
//  type Answer = Int //e.g. count as an example
//  def combine(a1:Rep[Answer], a2:Rep[Answer]) = a1 + a2
//}

trait CharRepParsers extends AGCharParsers {
  //type Answer = Char
  //implicit val ansManifest:Manifest[Char] //import scala.reflect.{ SourceContext, RefinedManifest }
  def Pars: AGParser[Char] = {
    acceptIf(x => x == unit('h')) //map ( _ + 2) //test basic functionality
  }
}

trait CharRepParsersExp extends AGCharParsersExp

trait GenCharRepParsers extends ScalaGenAGCharParsers {
  val IR: CharRepParsersExp
}

trait CharRepParsersProg extends CharRepParsers {
  import AGParser._ //for phrase

  def acceptIf(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val parser = super.acceptIf(x => x == unit('h'))
    val defAns:Rep[Answer] = unit('h')
    phrase(parser, StringReader(in), defAns)
  }
}


object StagedTest extends App {
  (new CharRepParsersProg //StagedParsersProg
    with CharRepParsersExp
    with MyIfThenElseExpOpt //IfThenElseExpOpt
    with StructOpsFatExpOptCommon
    with MyScalaCompile {
    self =>

    val codegen = new GenCharRepParsers with ScalaGenFatStructOps with MyScalaGenIfThenElseFat {
      val IR: self.type = self
    }

    //codegen.emitSource(acceptIf _, "acceptIf", new java.io.PrintWriter(System.out))
    //codegen.reset
    val testcAcceptIf = compile(acceptIf)
    scala.Console.println(testcAcceptIf("hello".toArray))
    scala.Console.println(testcAcceptIf("ello".toArray))
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
//       * Attention: Need to mix in Fat versions of StructOps as well as IfthenElse
//       * for optimisations on FatIfs and so on.
//       * Note: We are also using our own version of IfThenElseGenFat
//       * to generate variables instead of tuples and boundary ends
//       * of conditional expressions.
//       */
//      new StagedParsersProg
//        with CharParsersExp
//        with MyIfThenElseExpOpt //IfThenElseExpOpt
//        with StructOpsFatExpOptCommon
//        with MyScalaCompile {
//        self =>
//
//        val codegen = new ScalaGenCharParsers with ScalaGenFatStructOps with MyScalaGenIfThenElseFat {
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
