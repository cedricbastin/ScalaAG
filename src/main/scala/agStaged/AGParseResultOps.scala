//package agStaged
//
//import scala.virtualization.lms.common.Base
//import scala.util.parsing.combinator.syntactical.Input
//
///**
// * Created by cedricbastin on 11/05/15.
// */
//class AGParseResultOps extends Base {
//  def unit[T:Manifest](x: T): Rep[T] = {
//    null.asInstanceOf[Rep[T]] //why do all other *Ops not declare this method?
//  }
//
//  abstract class ParseResult[+T: Manifest] {
//    def isEmpty: Rep[Boolean]
//    def next: Rep[Input]
//    def res: Rep[T]
//  }
//
//}
