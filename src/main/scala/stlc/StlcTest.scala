package stlc

import scala.collection.immutable.HashMap

/**
 * Created by cedricbastin on 30/04/15.
 */
object TypingTest extends App {
//  class StlcTypingTest extends StlcGrammar with SltcTypingAlgebra {
//    val tests = Source.fromFile("test.in").getLines.filter(!_.startsWith("/*")).toList
//    tests.foreach {
//      testString =>
//        val tokens = new lexical.Scanner(testString)
//        print(testString + " --> ")
//        val defans:AttrEnv = new HashMap()
//
//        val parsed = Term(defans, tokens)
//        parsed match {
//          case AGSuccess(res, next, ans) =>
//            println("answer env:"+ans)
//            println(res)
//          case e =>
//            println(e)
//        }
//    }
//  }
//  val test = new StlcTypingTest()

  object StlcTypingTest extends StlcGrammar with SltcTypingAlgebra {
    def test(in:String, tpe:Type) = {
      println(in)
      val tokens = new lexical.Scanner(in)
      val initAns:AttrEnv = new HashMap()
      val parsed = Term(initAns, tokens)
      parsed match {
        case AGSuccess(res, next, ans) =>
          if (res == tpe) println("  =>  "+tpe)
          else println("type mismatch: expected: "+tpe+" found:"+res)
        case e =>
          println("parsing failure: "+e)
      }
    }

    def testAll() = {
//      test("true", TypeBool)
//      test("0", TypeNat)
//      test("3", TypeNat)
//      test("pred pred 0", TypeNat)
//      test("succ 0", TypeNat)
//      test("pred 4", TypeNat)
      test("if true then 0 else succ 1", TypeNat)
      //ABSTRACTIONS
      test("""(\x:Nat. x)""", TypeFun(TypeNat, TypeNat))
      test("""(\x:Nat. iszero x)""", TypeFun(TypeNat, TypeBool))
      test("""(\x:Bool. if x then 0 else succ 1) false""", TypeNat)
      test("""(\x:Nat. pred x) succ 5""", TypeNat)
      test("""(\x:Bool.\y:Nat.\z:Nat. if x then y else succ(z))""", TypeFun(TypeBool, TypeFun(TypeNat, TypeFun(TypeNat, TypeNat))))
      //FIRST ORDER FUNCTIONS
      test("""(\f:(Nat -> Bool). 0)""", TypeFun(TypeFun(TypeNat, TypeBool), TypeNat))
      test("""(\f:Nat -> Bool. f 0) (\x:Nat. iszero x )""", TypeBool)
      //VARIABLE MASKING?
      test("""(\x:Nat. (\x:Bool. x) iszero x)""", TypeFun(TypeNat, TypeBool))
      test("""(\x:Nat. if true then x else ((\x:Bool. 3) true))""", TypeFun(TypeNat, TypeNat))
      test("""if true then (\x:Bool. x) else (\y:Bool. y)""", TypeFun(TypeBool, TypeBool))
    }
  }
  StlcTypingTest.testAll()
}
