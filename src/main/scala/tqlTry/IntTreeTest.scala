package tqlTry

import scala.language.experimental.macros
import tql._
import scala.meta.tql.ScalaMetaTraverser._


object IntTreeTest {
  
  object IntMono extends tql.Monoid[Int] {
    def zero = 0
    def append(a: Int, b: Int) = a + b
  }
  
  trait IntTree
  case class Node(e:IntTree, l:IntTree) extends IntTree
  case class Leaf(i:Int) extends IntTree
  
  val intTree = Node(Node(Leaf(1), Leaf(2)), Node(Node(Leaf(3), Leaf(4)), Leaf(5)))
  
   //tqlscalameta/src/main/scala/meta/tql/ScalaMetaTraverser.scala
//   implicit def materializerAllowedTransformation[I <: Tree, O <: Tree]: AllowedTransformation[I, O] =
//    macro AllowedTransformationsMaterializer.materialize[scala.meta.internal.ast.Tree, I, O]
  //val fsd = materializerAllowedTransformation[IntTree, IntTree]
  
//  val matcher = collect{case Leaf(i) => i} //Matcher[C[A]]
//  val matcherOfMonoid = topDown(matcher)
  
}