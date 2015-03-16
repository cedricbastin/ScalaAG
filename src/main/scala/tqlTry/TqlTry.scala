package tqlTry

import scala.meta.tql.ScalaMetaTraverser._
import scala.meta.tql._
import tql._ //why does this work/exists?

import scala.meta.internal.ast._
import scala.meta.ui._
import scala.meta.syntactic._
import scala.language.reflectiveCalls
import scala.meta.dialects.Scala211

import scala.language.experimental.macros
//import scala.meta._

//TQL
// combinator functions = matchers 
//                                          = Base Combinators (collect, transform, focus)
//tql/src/main/scala/tql/Combinators.scala  = Traversal Combinators 
//tql/src/main/scala/tql/tql.scala          = Glue combinators

//reduce[A](op: (A, A) ⇒ A): A //should be commutative!
//reduceLeft[B >: A](f: (B, A) ⇒ B): B
//fold[A](z: A)(op: (A, A) ⇒ A): A //with initial value!
//foldLeft[B](z: B)(f: (B, A) ⇒ B): B //with initial value!

//your programming language forms a category
//types as the objects
//functions as the arrows
//'map' is the action of a functor on arrows
//'reduce' is an ordered fold, which is (again roughly) the 'bind' of a monad

//trait Monoid[A]{
//  def zero : A
//  def append(a: A, b: A): A
//}
//implicit object Void extends Monoid[Unit]{
//    def zero = ()
//    def append(a: Unit, b: Unit) = ()
//  }

//Matcher is a Monad?

//abstract class Matcher[+A] extends (T => MatchResult[A]) {
//type MatchResult[A] = Option[(T, A)] // A = Monoid?
//def collect[C[_]]{
//  def apply[A](f: PartialFunction[T, A]): Matcher[C[A]] //or Matcher[List[A]] if C[_] is not specified
//}
//def children[A : Monoid](f: Matcher[A]): Matcher[A]
//def topDown [A : Monoid](m: Matcher[A]): Matcher[A]

case object IntMono extends tql.Monoid[Int] {
    def zero = 0
    def append(a: Int, b: Int) = a + b
  }

object TqlTry extends App {  
// ###SCALA META EXAMPLE###
  //visit{...}.downBreak(addMonoid)
   val x =
    q"""
       val a = 5
       val b = a + 4
       """
  implicit def stringMonoid = new Monoid[String] {
    def zero = ""
    def append(a: String, b: String) = a + b
  }

   val col = visit{
     //case x:Decl => "assign"
     case x => ""+x.getClass+"\r\n"
   }
   val bu = col.bottomUp
   val res = bu(x)
   println(res)
   
   //meta defs: scalameta/scalameta/src/main/scala/scala/meta/Trees.scala

    
//visit({...})(addMonoid).downBreak
//visit[IntMonoid]{..}.downBreak //syntax not implemented
//visit{...}.downBreak(addMonoid) //no syntax existing?
   
  //def combine[B](x: Matcher[B]):EvaluatorAndThen[Term.Block, B] = topDown.combine(x)
  //val y = x.bottomUp.combine{collect{case l@Lit.Int(a)  => IntMono.(_,_)}} //case Lit.Int(a) if a > 10 => a
//  println(y.result)
//   val a = 4; val b = 3 + a
//   val h = setM
   
   


//    val inter:Matcher[List[Int]] =
      //transform{case Lit.Int(a) => println(a); a} +
//      children(collect{case Lit.Int(a) => println(a); a})
  //inter map (_.map(_ * 10))
//  println("even: "+inter(x).result)
  //topDown{case l@Lit.Int(i) => Some(l,i)}
  //m | children(topDownBreak(IntMono))

}