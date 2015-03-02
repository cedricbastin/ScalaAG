/**
 * Created by Eric on 20.10.2014.
 */

import scala.meta.tql.ScalaMetaTraverser._
import scala.meta.internal.ast._
import scala.meta.ui._
import scala.meta.syntactic._
import scala.language.reflectiveCalls
import scala.meta.dialects.Scala211

object Example extends App {


  val x =
    q"""
       val a = 5
       val c = 3 + a
       """
    println(x)
  
  //val y = x | children(topDownBreak[A](x))
  val t1: List[Int] = x.collect{case Lit.Int(a) if a > 10 => a}
  println(t1)

//  val getAllVals = (collect[Set]{case x: Defn.Val => x.pats.head.toString}).topDown
//
//  val listToSetBool = topDown(transform{  //WithResult[Term.Apply, Term.Select, List[String]]
//    case tt @ Term.Apply(t @ Term.Select(Term.Apply(Term.Name("List"), _), Term.Name("toSet")), _) =>
//      t andCollect tt.toString
//  })
//
//  val test = transform {
//    case Lit.Int(a) => Lit.Int(a * 3)
//    case Defn.Val(a, b, c, d) => Defn.Var(a,b,c,Some(d))
//  }.topDown
//
//
//  val t1: List[Int] = x.collect{case Lit.Int(a) if a > 10 => a}
//
//  val t2: List[Int] = x.focus({case Term.If(_,_,_) => true}).topDown.collect{case Lit.Int(a) => a}
//  val t3: (scala.meta.Tree, List[String]) = x.transform{case Defn.Val(a, b, c, d) => Defn.Var(a,b,c,Some(d)) andCollect(b.toString)}
//  val t4: scala.meta.Tree = tree.transform{case Lit.Int(x) => Lit.Int(x * 2)}
//  val t5: Set[String] = x.bottomUp.collect[Set]{case x: Defn.Val => x.pats.head.toString}
//  val t6: List[Int] = x.focus({case Term.If(_,_,_) => true}).combine(topDown(collect{case Lit.Int(a) => a})).result
//  val t7: scala.meta.Tree = x.transform {
//    case Lit.Int(a) => Lit.Int(a * 3)
//    case Defn.Val(a, b, c, d) => Defn.Var(a,b,c,Some(d))
//  }
//
//  val xOpt: Option[scala.meta.Tree] = Some(x)
//  val t8 = xOpt.transform{case Lit.Int(x) => Lit.Int(x * 2)}
//  val xs = List(x, tree)
//  val t9 = xs.topDown.collect{case Lit.Int(a) => a} //here we need topDown since collect already exists on Lists
//
//  println(t6)
//
//  val hey = x \: focus{case _: Term.If => true} \: focus{case Lit.Int(x) => x > 2} \: collect{case Lit.Int(a) => a}
//
//  val testUntil = until(collect{case Lit.Int(a) => a}, focus{case _:Term.While => true})
//
//  val testAggregateUntil = tupledUntil(
//    collect{case Lit.Int(a) => a},
//    focus{case _:Term.While => true} ~> topDown(collect[Set]{case Lit.Int(a) => a * 2})
//  )
//
//  val fixtest = fix[List[Int]]{r =>
//    collect{case Lit.Int(x) => x}
//  }.topDown
//
//  println(fixtest)

}