package agPC

import java.io._
import java.util.Scanner

import scala.io.Source

object Tests extends scala.App {
  val file = new File("test.in")
  var res: List[String] = Nil
  val sc = new Scanner(file)

  val tests = Source.fromFile("test.in").getLines.filter(!_.startsWith("/*")).toList
  println(tests)

  //(new agPC.TwoPhaseInferencer).typeOf(term)

  //val lines: Infer.lexical.Scanner = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
}
