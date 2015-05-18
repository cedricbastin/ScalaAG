//package agStaged
//
////local functadelic
//
//import stagedparsec._
//import lms._
//import lms.Pacl
//
//import scala.virtualization.lms.common._
//import scala.virtualization.lms.internal.Effects
//
//import org.scalatest.Suite
//
//import java.io.PrintWriter
//import java.io.StringWriter
//import java.io.FileOutputStream
//
///**
// * Created by cedricbastin on 18/05/15.
// */
//trait AGParseResultTest extends ParseResultOps {
//  //with MyScalaOpsPkg
//
//  def test1(in: Rep[Int]): Rep[Unit] = {
//    val s: Rep[ParseResult[Int]] = Success(in, unit(10))
//    //val t = s.map(x => x * unit(2))
//    println(s)
//  }
//
//  //    //a map
//  //    def testMap(in: Rep[Int]): Rep[Unit] = {
//  //      val s: Rep[ParseResult[Int]] = Success(in, unit(10))
//  //      val t = s.map(x => x * unit(2))
//  //      println(t)
//  //    }
//  //
//  //    //a map of Fail
//  //    def testMapFail(in: Rep[Int]): Rep[Unit] = {
//  //      val s: Rep[ParseResult[Int]] = Failure[Int](in)
//  //      val t = s.map(x => x * unit(2))
//  //      println(t)
//  //    }
//  //
//  //    //orElse left
//  //    def testOrElseLeft(in: Rep[Int]): Rep[Unit] = {
//  //      val s: Rep[ParseResult[Int]] = Failure[Int](in)
//  //      val t = s orElse Success(unit(2), in)
//  //      println(t)
//  //    }
//  //
//  //    //orElse right
//  //    def testOrElseRight(in: Rep[Int]): Rep[Unit] = {
//  //      val s: Rep[ParseResult[Int]] = Success[Int](in, unit(10))
//  //      val t = s orElse Success(unit(2), in)
//  //      println(t)
//  //    }
//}
//
//trait MyScalaOpsPkg extends Base
//with ImplicitOps with NumericOps with FractionalOps with OrderingOps with StringOps
//with RangeOps with IOOps with ArrayOps with BooleanOps with PrimitiveOps with MiscOps
//with Equal with IfThenElse with Variables with While with MyTupleOps with ListOps
//with SeqOps with MathOps with CastingOps with SetOps with ObjectOps with ArrayBufferOps
//
//trait MyScalaOpsPkgExp extends MyScalaOpsPkg
//with ImplicitOpsExp with NumericOpsExp with FractionalOpsExp with OrderingOpsExp with StringOpsExp
//with RangeOpsExp with IOOpsExp with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp
//with FunctionsExp with EqualExp with IfThenElseExp with VariablesExp with WhileExp with MyTupleOpsExp with ListOpsExp
//with SeqOpsExp with DSLOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp with ArrayBufferOpsExp
//
//trait MyScalaCodeGenPkg extends ScalaGenImplicitOps with ScalaGenNumericOps with ScalaGenFractionalOps with ScalaGenOrderingOps
//with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps with ScalaGenArrayOps with ScalaGenBooleanOps
//with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenFunctions with ScalaGenEqual with ScalaGenIfThenElse
//with ScalaGenVariables with ScalaGenWhile with ScalaGenMyTupleOps with ScalaGenListOps
//with ScalaGenSeqOps with ScalaGenDSLOps with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenSetOps
//with ScalaGenObjectOps with ScalaGenArrayBufferOps {
//  val IR: MyScalaOpsPkgExp
//}
//
//class TestParseResultOps extends Suite {
//  //extends FileDiffSuite
//
//  val prefix = "test-out/"
//
//  val file = new java.io.File(prefix + "parseresult")
//
////  def assertFileEqualsCheck(name: String): Unit = {
////        expectResult(readFile(name+".check")){readFile(name)}
////        new File(name) delete ()
////      }
//
//  def testOption = {
//    val f = new AGParseResultTest with ParseResultOpsExp with IfThenElseExpOpt with StructOpsExpOptCommon with MyScalaCompile with MyScalaOpsPkgExp {
//      self =>
//
//      val codegen = new MyScalaCodeGenPkg with ScalaGenParseResultOps
//        with lms.ScalaGenStructOps {
//        val IR: self.type = self
//      }
//
////        codegen.emitSource(test1 _, "test1", new java.io.PrintWriter(System.out))
////        val testc1 = compile(test1)
////        testc1(3)
////
////        codegen.emitSource(testMap _, "testMap", new java.io.PrintWriter(System.out))
////        val testcMap = compile(testMap)
////        testcMap(3)
////
////        codegen.emitSource(testMapFail _, "testMapFail", new java.io.PrintWriter(System.out))
////        val testcMapFail = compile(testMapFail)
////        testcMapFail(3)
////
////        codegen.emitSource(testOrElseLeft _, "testOrElseLeft", new java.io.PrintWriter(System.out))
////        val testcOrElseLeft = compile(testOrElseLeft)
////        testcOrElseLeft(3)
////
////        codegen.emitSource(testOrElseRight _, "testOrElseRight", new java.io.PrintWriter(System.out))
////        val testcOrElseRight = compile(testOrElseRight)
////        testcOrElseRight(3)
//
//    }
//
////    assertFileEqualsCheck(prefix + "parseresult")
//  }
//}