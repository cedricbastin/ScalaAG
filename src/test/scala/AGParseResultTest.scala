import agStaged._
import lms._
import stagedparsec._

trait StagedSig extends AGSig { //TODO: also Rep[]?
  def start(tag:String, a:Answer):Answer
}

trait StagedGrammar extends AGStagedParsers with StagedSig {
  def Pars: AGParser[Int] = {
      lift(digit2Int) map ( _ + 2) //test basic functionality
  }
}

trait StagedAlgebra extends StagedSig {
  type Answer = Int //e.g. count as an example
  def combine(a1:Answer, a2: Answer) = a1 + a2
}

trait CharParsersProg extends CharParsers {
  import Parser._

  def acceptIf(in: Rep[Array[Char]]): Rep[Option[Char]] = {
    val parser = acceptIf(x => x == unit('h'))
    phrase(parser, StringReader(in))
  }
}

object Test extends App {
  (new CharParsersProg
    with CharParsersExp
    with MyIfThenElseExpOpt //IfThenElseExpOpt
    with StructFatExpOptCommon
    with MyScalaCompile {
    self =>

    val codegen = new ScalaGenCharParsers with ScalaGenFatStruct with MyScalaGenIfThenElseFat {
      val IR: self.type = self
    }

    //codegen.emitSource(acceptIf _, "acceptIf", new java.io.PrintWriter(System.out))
    //codegen.reset

  }).acceptIf("e") fsdjkvkjnkj
}

class CharParsersSuite extends FileDiffSuite {

  val prefix = "test-out/"

  def testCharParsers = {
    withOutFile(prefix + "char-parser") {
      /**
       * Attention: Need to mix in Fat versions of Struct as well as IfthenElse
       * for optimisations on FatIfs and so on.
       * Note: We are also using our own version of IfThenElseGenFat
       * to generate variables instead of tuples and boundary ends
       * of conditional expressions.
       */
      new CharParsersProg
        with CharParsersExp
        with MyIfThenElseExpOpt //IfThenElseExpOpt
        with StructFatExpOptCommon
        with MyScalaCompile {
        self =>

        val codegen = new ScalaGenCharParsers with ScalaGenFatStruct with MyScalaGenIfThenElseFat {
          val IR: self.type = self
        }

        codegen.emitSource(acceptIf _, "acceptIf", new java.io.PrintWriter(System.out))
        codegen.reset

      }
      assertFileEqualsCheck(prefix + "char-parser")
    }
  }
}

//class StagedAGSuite extends FileDiffSuite with StagedGrammar with StagedAlgebra {
//
//  def test(in:Rep[Array[Char]]) = {
//    print(in+"=> ")
//    val initAns = unit(0)
//    Pars(initAns, StringReader(in))
//
//    //print parsed result or something
//  }
//
//}
//
//object Test extends App {
//
//  new StagedAlgebra
//    with CharParsersExp {
//    self =>
//
//    val codegen = new ScalaGenCharParsers with ScalaGenFatStruct with MyScalaGenIfThenElseFat {
//      val IR: self.type = self
//    }
//  }
//
////  new StagedTest()
//}
