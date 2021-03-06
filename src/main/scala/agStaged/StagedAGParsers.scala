package agStaged

import scala.virtualization.lms.common._

////local functadelic:
import stagedparsec._ //for ReaderOps
import lms._
import lms.util._

trait AGSig extends Base {
  type Answer
  implicit val ansManifest: Manifest[Answer] // = implicitly[Manifest[Answer]]//manifest[Answer] //needed by lms
  //def combine(a1:Rep[Answer], a2:Rep[Answer]):Rep[Answer] //TODO: put back
}

trait AGStagedParsers extends AGParseResultOps with AGSig with OptionOps with ReaderOps with MyTupleOps  {


  //T is independent from Answer but can be included as additional information in an Answer if needed
  // parser combinator methods arguments need to be call by name otherwise the stack will overflow
  abstract class AGParser[+T: Manifest] extends ((Rep[Answer], Rep[Input]) => Rep[AGParseResult[T]]) { //tuple of rep rather than rep of tuple

//  def map[U: Manifest](f: Rep[T] => Rep[U]) = Parser[U] { input =>
//    this(input) map f
//  }

    def map[U: Manifest](f: Rep[T] => Rep[U]) = AGParser[U] {
      (ans: Rep[Answer], input: Rep[Input]) =>
        val tmp = this(ans, input)
        if (tmp.isEmpty)
          AGFailure[U](tmp.next)
        else
          AGSuccess(f(tmp.get), tmp.next, ans)
    }
//
////    def ^^[U](f: Rep[T] => Rep[U]) = map(f)
//
////    def ^^^[U](f: => Rep[U]) = AGParser[U] {
////      //include Answer in the signature?
////      case (ans: Answer, input: Input) =>
////        this(ans, input) match {
////          case AGSuccess(result1, next1, _) =>
////            AGSuccess(f, next1, ans)
////          case AGFailure(msg1, next1) =>
////            AGFailure(msg1, next1)
////        }
////    }
////
////    def mapWithAns[U](f: (Rep[T], Answer) => Rep[U]) = AGParser[U] {
////      case (ans: Answer, input: Input) =>
////        this(ans, input) match {
////          case AGSuccess(result1, next1, ans1) =>
////            AGSuccess(f(result1, ans1), next1, ans)
////          case AGFailure(msg1, next1) =>
////            AGFailure(msg1, next1)
////        }
////    }
////
////    def >>^^[U](f: (Rep[T], Answer) => Rep[U]) = mapWithAns(f)
////
////    def mapIntoAns[U](f: Rep[T] => Rep[U], add:(Answer, Rep[U]) => Answer) = AGParser[U] {
////      case (ans: Answer, input: Input) =>
////        this(ans, input) match {
////          case AGSuccess(result1, next1, _) =>
////            val res = f(result1)
////            AGSuccess(res, next1, add(ans, res))
////          case AGFailure(msg1, next1) =>
////            AGFailure(msg1, next1)
////        }
////    }
////
////    def
////    ^^>>[U](f: Rep[T] => Rep[U], add:(Answer, Rep[U]) => Answer) = mapIntoAns(f, add)
////
////    def mapWithAnsIntoAns[U](f:(Rep[T], Answer) => (Answer, Rep[U])) = AGParser[U]  {
////      case (ans: Answer, input: Input) =>
////        this(ans, input) match {
////          case AGSuccess(result1, next1, ans1) =>
////            val res = f(result1, ans1)
////            AGSuccess(res._2, next1, res._1)
////          case AGFailure(msg1, next1) =>
////            AGFailure(msg1, next1)
////        }
////    }
//
////    def >>^^>>[U](f: (T, Answer) => (Answer, U)) = mapWithAnsIntoAns(f)
//
////    def flatMap[U](f: T => AGParser[U]) = AGParser[U] {
////      case (ans: Answer, input: Input) =>
////        this(ans, input) match {
////          case AGSuccess(result1, next1, ans1) =>
////            f(result1)(ans1, next1)
////          case AGFailure(msg1, next1) =>
////            AGFailure(msg1, next1)
////        }
////    }
//
////    def >>[U](f: T => AGParser[U]) = flatMap(f)
////    def into[U](f: T => AGParser[U]) = flatMap(f)
//
////    def flatMapWithAns[U](f: (T, Answer) => AGParser[U]) = AGParser[U] {
////      case (ans: Answer, input: Input) =>
////        this(ans, input) match {
////          case AGSuccess(result1, next1, ans1) =>
////            f(result1, ans1)(ans1, next1)
////          case AGFailure(msg1, next1) =>
////            AGFailure(msg1, next1)
////        }
////    }
////
////    def |[U >: T](that: => AGParser[U]) = AGParser[U] {
////      case (ans: Answer, input: Input) =>
////        this(ans, input) match {
////          case s:AGSuccess[U] => s //return result of first parser
////          case _ => that(ans, input) //or apply the second parser
////        }
////    }
//
//    /*
//     * sequence parsers return "~" tuples to allow easy pattern matching over the AGParseResults
//     * in general only pipe original env through except for ~~
//     */
//
//    /**
//     * Never augement the answer, always pipe the original answer
//     * @param that
//     * @tparam U
//     * @return
//     */
////    def ~[U](that: => AGParser[U]) = AGParser[~[T, U]] {
////      case (ans: Answer, input: Input) =>
////        this(ans, input) match {
////          case AGSuccess(result1, next1, _) =>
////            that(ans, next1) match { //use original Answer
////              case AGSuccess(result2, next2, _) =>
////                AGSuccess(new ~(result1, result2), next2, ans) //return original Answer
////              case AGFailure(msg2, next2) =>
////                AGFailure(msg2, next2)
////            }
////          case AGFailure(msg1, next1) =>
////            AGFailure(msg1, next1)
////        }
////    }
////
////    /**
////     * pipe Answer from parser1 to parser2 but not don't output as the final answer
////     * @param that
////     * @tparam U
////     * @return
////     */
////    def >>~[U](that: => AGParser[U]) = AGParser[~[T, U]] {
////      case (ans: Answer, input: Input) =>
////        this(ans, input) match {
////          case AGSuccess(result1, next1, ans1) =>
////            that(ans1, next1) match { //use augmented answer from parser1
////              case AGSuccess(result2, next2, _) =>
////                AGSuccess(new ~(result1, result2), next2, ans) //return initial answer
////              case AGFailure(msg2, next2) =>
////                AGFailure(msg2, next2)
////            }
////          case AGFailure(msg1, next1) =>
////            AGFailure(msg1, next1)
////        }
////    }
////
////    /**
////     * pipe Answer from parser2 to global environment of the combined parser
////     * @param that
////     * @tparam U
////     * @return
////     */
////    def ~>>[U](that: => AGParser[U]) = AGParser[~[T, U]] {
////      case (ans: Answer, input: Input) =>
////        this(ans, input) match {
////          case AGSuccess(result1, next1, _) =>
////            that(ans, next1) match { //use original answer
////              case AGSuccess(result2, next2, ans2) =>
////                AGSuccess(new ~(result1, result2), next2, ans2) //return answer from 2nd parser
////              case AGFailure(msg2, next2) =>
////                AGFailure(msg2, next2)
////            }
////          case AGFailure(msg1, next1) =>
////            AGFailure(msg1, next1)
////        }
////    }
////
////    /**
////     * pipe answer1 to parser2 and pipe answer2 to final result
////     * @param that
////     * @tparam U
////     * @return
////     */
////    def >>~>>[U](that: => AGParser[U]) = AGParser[~[T, U]] {
////      case (ans: Answer, input: Input) =>
////        this(ans, input) match {
////          case AGSuccess(result1, next1, ans1) =>
////            that(ans1, next1) match { //use new answer
////              case AGSuccess(result2, next2, ans2) =>
////                AGSuccess(new ~(result1, result2), next2, ans2) //return answer from 2nd parser
////              case AGFailure(msg2, next2) =>
////                AGFailure(msg2, next2)
////            }
////          case AGFailure(msg1, next1) =>
////            AGFailure(msg1, next1)
////        }
////    }
////
////    def ~>[U](that: => AGParser[U]) = AGParser[U] {
////      case (ans: Answer, input: Input) => this.~(that)(ans, input) match {
////        case AGSuccess(~(t: T, u: U), next, _) =>
////          AGSuccess[U](u, next, ans)
////        case AGFailure(msg1, next1) =>
////          AGFailure(msg1, next1)
////      }
////    }
////
////    def <~[U](that: => AGParser[U]) = AGParser[T] {
////      case (ans: Answer, input: Input) => this.~(that)(ans, input) match {
////        case AGSuccess(~(t: T, u: U), next, _) =>
////          AGSuccess[T](t, next, ans)
////        case AGFailure(msg1, next1) =>
////          AGFailure(msg1, next1)
////      }
////    }
////
////    def failureAG(s:String) = AGParser[T] {
////      case (ans: Answer, input: Input) =>
////        failure(s)(input) match {
////          case Success(result, next) => AGSuccess[T](result, next, ans)
////          case Failure(msg, next) => AGFailure(msg, next)
////        }
////    }


  }

  //help functions for Parsers trait
//
//TODO: lift can only be defined once a base parser exists!!
//  def lift[T:Manifest](pars: => scala.util.parsing.combinator.Parser[T]) = AGParser[T] {
//    case (ans: Rep[Answer], input: Rep[Input]) =>
//      val stag = pars(input)
//      if (parseresult_isEmpty(stag)) AGFailure(stag.next)
//        else AGSuccess[T](stag.get, stag.next, ans)
//  }
//  //implicit def impLift(s:String) = lift(keyword(s))
//  //implicit def impLift[T](p: Parser[T]) = lift(p)
//
//
////  def rep[T](pars: AGParser[T]):AGParser[List[T]] = AGParser[List[T]] {
////    case (ans: Answer, input: Input) =>
////      pars(ans, input) match {
////        case AGSuccess(result1, next1, ans1) =>
////          rep(pars)(ans, next1) match {
////            //pipe original Answer
////            case AGSuccess(result2, next2, ans2) => AGSuccess(result1 :: result2, next1, ans) //propagate old Answer
////            case fail => AGSuccess(result1 :: Nil, next1, ans)
////          }
////        case AGFailure(msg1, next1) => AGSuccess(Nil, next1, ans) //rep parser always suceeds
////      }
////  }
////
////  def repWithAns[T](pars: AGParser[T]):AGParser[List[T]] = AGParser[List[T]] {
////    case (ans: Answer, input: Input) =>
////      pars(ans, input) match {
////        case AGSuccess(result1, next1, ans1) =>
////          rep(pars)(ans1, next1) match {
////            //pipe original Answer
////            case AGSuccess(result2, next2, ans2) => AGSuccess(result1 :: result2, next1, ans2) //propagate old Answer
////            case fail => AGSuccess(result1 :: Nil, next1, ans)
////          }
////        case AGFailure(msg1, next1) => AGSuccess(Nil, next1, ans) //rep parser always suceeds
////      }
////  }

  object AGParser {
    //companion object to build new parsers without the need for and "val outer = this" reference
    def apply[V: Manifest](f: (Rep[Answer], Rep[Input]) => Rep[AGParseResult[V]]) = new AGParser[V] {
      def apply(ans: Rep[Answer], input: Rep[Input]) = f(ans, input)
    }


    //what does this do?
    def phrase[T: Manifest](p: => AGParser[T], in: Rep[Input], ans:Rep[Answer]): Rep[Option[T]] = {
      val presult = p(ans, in)
      val res = if (presult.isEmpty) none[T]() else Some(presult.get)
      res
    }
  }
}

trait AGStagedParsersExp
  extends AGStagedParsers
  with AGParseResultOpsExp
  with OptionOpsExp
  with MyTupleOpsExp
  with IfThenElseExpOpt
  with BooleanOpsExpOpt
  with EqualExpOpt


trait ScalaGenAGStagedParsers
  extends ScalaGenParseResultOps
  with ScalaGenOptionOps
  with ScalaGenMyTupleOps
  with ScalaGenIfThenElse
  with ScalaGenBooleanOps
  with ScalaGenEqual {
  val IR: AGStagedParsersExp
}
