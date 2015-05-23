package repmin

import agParsers._

/**
 * Created by cedricbastin on 23/05/15.
 */
sealed trait Tree
case class Node(l:Tree, v:Int, r:Tree) extends Tree
case object Leaf extends Tree


trait RepMinSig extends AGSig {
  def node(a1:Tree, i:Answer, a2:Tree):Tree
  def leaf: Tree
  def toAns(s:String):Answer
  def get(a:Answer):Int
}

trait RepMinAlgebra extends RepMinSig {
  type Answer = Int//find min in one direction and
  //type AnswerX = Elem => Tree
//  def caca = {
//    def rec(t: Tree, min: => Int):(() => Tree, Int) = {
//      t match {
//        case Node(tl, v, tr) =>
//          val l = rec(tl, min)
//          val r = rec(tr, min)
//          //the minima from both subnodes are available and are thus concrete
//          val cmin = Math.min(v, Math.min(l._2, r._2))
//          (() => Node(l._1(), min, r._1()), cmin)
//        case Leaf => (() => Leaf, Int.MaxValue) //initial empty acc
//      }
//    }
//    // as min is a recursive value it need a type
//    lazy val (res, min:Int) = rec(tree, min)
//    // evaluate the lazy tree as min is now available
//    res()
//  }

  def node(l:Tree, a:Answer, r:Tree): Tree = Node(l, a, r)
  def leaf = Leaf

  def toAns(s:String):Answer = s.toInt
  def get(a:Answer) = a
  def combine(a1:Answer, a2:Answer):Answer = if (a1 < a2) a1 else a2
}

trait RepMinGrammar extends AGParsers with RepMinSig {
  lexical.delimiters ++= List("(", ")")
  lexical.reserved ++= List("x")

  def TreeP:AGParser[Tree] = {
    NodeP |
      LeafP |
      lift(failure("illegal start of container"))
  }
  def NodeP:AGParser[Tree] = {
    lift("(") >>~>> TreeP >>~>> ValP >>~>> TreeP >>~>> lift(")") >>^^>> {
      case (s1 ~ a1 ~ a ~ a2 ~ s2, ans) =>
        val min = combine(ans, a)
        (min, node(a1.asInstanceOf[Tree], min, a2.asInstanceOf[Tree]))
    }
  }
  def LeafP:AGParser[Tree] = { //or simply "Tree" ?
    lift("x") ^^^ leaf
  }

  def toInt(s:String) = s.toInt

  def ValP:AGParser[Answer] = {
    lift(numericLit) >>^^>> {
      case (s, ans) =>
        val res = combine(ans, toAns(s))
        (res, res)
    }
  }
}

class RepMinTest extends RepMinGrammar with RepMinAlgebra {
  def test(s:String) = {
    val tokens:lexical.Scanner = new lexical.Scanner(s)
    val defAns = 1000
    val parsed:AGParseResult[Tree] = TreeP(defAns, tokens.asInstanceOf[Input])
    println(parsed)
  }
  def testAll() = {
    test("x")
    test("(x4x)")
    test("( x 4 x )")
    test("( ( x 3 x ) 4 x )")
    test("( ( ( x 4 x ) 5 ( x 3 x ) ) 4 ( x 2 x ) )")
  }
}

object Test extends App {
  new RepMinTest().testAll()
}
