package repmin

import agParsers._

/**
 * Created by cedricbastin on 23/05/15.
 */
sealed trait Tree {
  def print(s:String, flag:Boolean):Unit = this match {
    case Node(l, v, r) =>
      if (flag) {
        println(s+"├───" + v)
        l.print(s+"|   ", true)
        r.print(s+"|   ", false)
      } else {
        println(s+"└───" + v)
        l.print(s+"    ", true)
        r.print(s+"    ", false)
      }
    case Leaf =>
      if (flag)
        println(s+"├────X")
      else
        println(s+"└────X")
  }
}
case class Node(l:Tree, v:Int, r:Tree) extends Tree
case object Leaf extends Tree

//example to show that parsing is from left to right and evaluation can be from right to left
//parses a tree with integer values and return the tree
trait RepMinSig extends AGSig {
  type Attr = AttrEnv => Tree //unfold when minimum is found
  
  def node(a1:Attr, i:AttrEnv, a2:Attr):Attr
  def leaf: Attr

  def toAns(s:String):AttrEnv
}

trait RepMinAlgebra extends RepMinSig {
  type AttrEnv = Int

  def node(l:Attr, a:AttrEnv, r:Attr): Attr = {case i:Int => Node(l(i), i, r(i))} //unflod with minimum
  def leaf = {case _:Int => Leaf}

  def toAns(s:String):AttrEnv = s.toInt
  def combine(a1:AttrEnv, a2:AttrEnv):AttrEnv = if (a1 < a2) a1 else a2
}

trait RepMinGrammar extends AGParsers with RepMinSig {
  lexical.delimiters ++= List("(", ")")
  lexical.reserved ++= List("x") //leaves

  def Root:AGParser[Tree] = { //fully evaluate at the end!
    TreeP >>^^ {case (t, ans) => t(ans)} |
      lift(failure("tree not parsable!"))
  }
  def TreeP:AGParser[Attr] = {NodeP | LeafP}
  def NodeP:AGParser[Attr] = {
    lift("(") >>~>> TreeP >>~>> ValP >>~>> TreeP >>~>> lift(")") >>^^>> {
      case (s1 ~ a1 ~ a ~ a2 ~ s2, ans) => (node(a1, a, a2), ans)
    }
  }
  def LeafP:AGParser[Attr] = {
    lift("x") ^^^ leaf
  }
  def ValP:AGParser[AttrEnv] = {
    lift(numericLit) ^^>> {
      case s =>
        def ans = toAns(s)
        (ans, ans)
    }
  }
}

class RepMinTest extends RepMinGrammar with RepMinAlgebra {
  def test(s:String) = {
    println("")
    val tokens:lexical.Scanner = new lexical.Scanner(s)
    val defAns = 1000 //maximal minimum
    val parsed:AGParseResult[Tree] = Root(defAns, tokens)
    println(s)
    parsed.map(_.print("", false))
  }

  def testAll() = {
    //test("x")
    //test("( x 4 x )")
    //test("( ( x 3 x ) 4 x )")
    test("( ( ( x 4 x ) 5 ( x 1 ( ( ( x 9 x ) 6 ( x 3 x ) ) 6 x ) ) ) 4 ( x 2 x ) )")
  }
}

object Test extends App {
  new RepMinTest().testAll()
}
