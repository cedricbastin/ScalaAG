package sltc

import scala.io.Source
import scala.collection.immutable.HashMap

/**
 * Created by cedricbastin on 30/04/15.
 */
object TypingTest extends App {
  class StlcTypingTest extends StlcGrammar with SltcTypingAlgebra {
    val tests = Source.fromFile("test.in").getLines.filter(!_.startsWith("/*")).toList
    tests.foreach {
      testString =>
        val tokens = new lexical.Scanner(testString)
        print(testString + " --> ")
        val defans = Answer(new HashMap(), TypeUnit)

        val parsed = Term(defans, tokens)
        parsed match {
          case AGSuccess(res, next, ans) =>
            println(res.tpe)
          case e =>
            println(e)
        }
    }
  }
  val test = new StlcTypingTest()
}