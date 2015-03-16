package tqlTry

import scala.meta.tql.ScalaMetaTraverser._
import tql._ //why does this work/exists?
import scala.meta.internal.ast._
import scala.meta.ui._
import scala.meta.syntactic._
import scala.language.reflectiveCalls
import scala.meta.dialects.Scala211

object ericExample extends App {

  val x =
    q"""
       val a = 5
       val c = 3
       c = 5
       if (3 == 17) {
        val c = 1
        while (a != c) {println(78)}
        val x = 14
        while (a != c) {println(85)}
       }
       else 2
       5
       """

  val t1: List[Int] = x.collect { case Lit.Int(a) if a > 10 => a }
  // -> List(17, 78, 14, 85)
  val t2: List[Int] = x.focus({ case Term.If(_, _, _) => true }).topDown.collect { case Lit.Int(a) => a }
  // -> List(3, 17, 1, 78, 14, 85, 2)
  //val t3: (scala.meta.Tree, List[String]) = x.transform{case Defn.Val(a, b, c, d) => Defn.Var(a,b,c,Some(d)) andCollect(b.toString)}
  //val t4: scala.meta.Tree = tree.transform{case Lit.Int(x) => Lit.Int(x * 2)}
  val t5: Set[String] = x.bottomUp.collect[Set] { case x: Defn.Val => x.pats.head.toString }
  // -> Set(a, c, x)
  val t6: List[Int] = x.focus({ case Term.If(_, _, _) => true }).combine(topDown(collect { case Lit.Int(a) => a })).result
  //-> List(3, 17, 1, 78, 14, 85, 2)
  val t7: scala.meta.Tree = x.transform {
    case Lit.Int(a)           => Lit.Int(a * 3)
    case Defn.Val(a, b, c, d) => Defn.Var(a, b, c, Some(d))
  }
  //-> full tree?

  println(t1)
  println(t2)
  println(t5)
  println(t6)
  println(t7)
}