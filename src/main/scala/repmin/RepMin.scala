package repmin

import agParsers._

/**
 * Created by cedricbastin on 23/05/15.
 */
sealed trait Tree
case class Node(l:Tree, v:Int, r:Tree) extends Tree
case object Leaf extends Tree

//example to show that parsing is from left to right and evaluation can be from right to left
//parses a tree with integer values and return the tree
trait RepMinSig extends AGSig {
  type TreeF = InAttrs => Tree //unfold when minimum is found

  def node(a1:TreeF, i:InAttrs, a2:TreeF):TreeF
  def leaf: TreeF

  def toAns(s:String):InAttrs
}

trait RepMinAlgebra extends RepMinSig {
  type InAttrs = Int

  def node(l:TreeF, a:InAttrs, r:TreeF): TreeF = {case i:Int => Node(l(i), i, r(i))} //unflod with minimum
  def leaf = {case _:Int => Leaf}

  def toAns(s:String):InAttrs = s.toInt
  def combine(a1:InAttrs, a2:InAttrs):InAttrs = if (a1 < a2) a1 else a2
}

trait RepMinGrammar extends AGParsers with RepMinSig {
  lexical.delimiters ++= List("(", ")")
  lexical.reserved ++= List("x") //leaves

  def Root:AGParser[Tree] = { //fully evaluate at the end!
    TreeP >>^^ {case (t, ans) => t(ans)} |
      lift(failure("tree not parsable!"))
  }
  def TreeP:AGParser[TreeF] = {NodeP | LeafP}
  def NodeP:AGParser[TreeF] = {
    lift("(") >>~>> TreeP >>~>> ValP >>~>> TreeP >>~>> lift(")") >>^^>> {
      case (s1 ~ a1 ~ a ~ a2 ~ s2, ans) => (combine(ans, a), node(a1, a, a2))
    }
  }
  def LeafP:AGParser[TreeF] = {
    lift("x") ^^^ leaf
  }
  def ValP:AGParser[InAttrs] = {
    lift(numericLit) >>^^>> {
      case (s, ans) =>
        val res = combine(ans, toAns(s)) //"add to environment"
        (res, res) //return value of the parser is not important
    }
  }
}

class RepMinTest extends RepMinGrammar with RepMinAlgebra {
  def test(s:String) = {
    val tokens:lexical.Scanner = new lexical.Scanner(s)
    val defAns = 1000
    val parsed:AGParseResult[Tree] = Root(defAns, tokens)
    println(parsed)
  }

  def testAll() = {
    test("x")
    test("( x 4 x )")
    test("( ( x 3 x ) 4 x )")
    test("( ( ( x 4 x ) 5 ( x 4 x ) ) 4 ( x 2 x ) )")
  }
}

object Test extends App {
  new RepMinTest().testAll()
}
