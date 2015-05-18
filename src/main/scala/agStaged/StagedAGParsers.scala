package agStaged

//import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.virtualization.lms.common._

//local functadelic
import stagedparsec._
import lms._
import lms.StructOps

trait AGSig {
  type Answer
  type AnswerF = Answer => Answer //lazily computed environment
  def combine(a1:Answer, a2:Answer):Answer
}

/*
* An partial implementation of a parser library to allow to compute semanic function over a parse tree at the same time as parsing
*/
trait AGParsers extends StagedParsers with AGSig with StructOpsExp {
  implicit def ansManifest: Manifest[Answer] //def because Answer is an abstract type

  // type Answer is an abstract data type which can be configures to contain differnent environments needed to execute the semantic functions
  // is used instead of carrying around an additional type parameter

  sealed abstract class AGParseResult[+T] // wrapper class for ParseResult[T] which cannot be extended
  object AGParseResult {
    def apply[T:Manifest](pr:ParseResult[T], ans:Answer) =
      if (pr.isEmpty) unit(AGSuccess(pr.res, pr.next, ans))//(Manifest[AGSuccess[T]])
      else unit(AGFailure("failure here", pr.next))//(Manifest[AGFailure])
  }
  def m[T](x:T)(implicit m: Manifest[T]) = m
  //TODO: class NoSuccess extends ParseResult, Failure extends NoSuccess, Error extends NoSuccess
  case class AGSuccess[+T](result: T, next: Rep[Input], ans: Answer) extends AGParseResult[T]
  case class AGFailure(msg: String, next: Rep[Input]) extends AGParseResult[Nothing]

//  sealed abstract class AGParseResult[+T]
////    extends Manifest {
////    // wrapper class for ParseResult[T] which cannot be extended
////    def runTimeClass = AGParseResult.getClass //Java class representation?
////  }
//  object AGParseResult {
//    def apply[T](pr:ParseResult[T], ans:Answer) = {
//      if (pr.isEmpty) AGFailure("fail", pr.next)
//      else AGSuccess(pr.res, pr.next, ans)
//    }
//  }
//
  case class AGSuccess[T](res: T, next: Input, ans: Answer) extends AGParseResult[T]
  def Success(ag: AGSuccess) = unit(ag)

  //def AGSuccess[T: Manifest](res: Rep[T], next: Rep[Input], ans: Answer): Rep[AGParseResult[T]]
  def AGSuccess[T: Manifest](res: Rep[T], next: Rep[Input], ans: Rep[Answer]): Exp[AGParseResult[T]] =
    struct(
      classTag[AGParseResult[T]],
      "res" -> res,
      "empty" -> unit(false),
      "next" -> next,
      "ans" -> ans
    )
//  def AGFailure(msg: String, next: Rep[Input]): Rep[AGParseResult[Nothing] = {
//
//  }

  //T is independent from Answer but can be included as additional information in an Answer if needed
  // parser combinator methods arguments need to be call by name otherwise the stack will overflow
  abstract class AGParser[+T] extends ((Rep[Answer], Rep[Input]) => Rep[AGParseResult[T]]) { //tuple of rep rather than rep of tuple

    def map[U](f: Rep[T] => Rep[U]) = AGParser[U] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, _) =>
            AGSuccess(f(result1), next1, ans)
          case AGFailure(msg1, next1) =>
            AGFailure(msg1, next1)
        }
    }

//    def ^^[U](f: Rep[T] => Rep[U]) = map(f)

//    def ^^^[U](f: => Rep[U]) = AGParser[U] {
//      //include Answer in the signature?
//      case (ans: Answer, input: Input) =>
//        this(ans, input) match {
//          case AGSuccess(result1, next1, _) =>
//            AGSuccess(f, next1, ans)
//          case AGFailure(msg1, next1) =>
//            AGFailure(msg1, next1)
//        }
//    }
//
//    def mapWithAns[U](f: (Rep[T], Answer) => Rep[U]) = AGParser[U] {
//      case (ans: Answer, input: Input) =>
//        this(ans, input) match {
//          case AGSuccess(result1, next1, ans1) =>
//            AGSuccess(f(result1, ans1), next1, ans)
//          case AGFailure(msg1, next1) =>
//            AGFailure(msg1, next1)
//        }
//    }
//
//    def >>^^[U](f: (Rep[T], Answer) => Rep[U]) = mapWithAns(f)
//
//    def mapIntoAns[U](f: Rep[T] => Rep[U], add:(Answer, Rep[U]) => Answer) = AGParser[U] {
//      case (ans: Answer, input: Input) =>
//        this(ans, input) match {
//          case AGSuccess(result1, next1, _) =>
//            val res = f(result1)
//            AGSuccess(res, next1, add(ans, res))
//          case AGFailure(msg1, next1) =>
//            AGFailure(msg1, next1)
//        }
//    }
//
//    def
//    ^^>>[U](f: Rep[T] => Rep[U], add:(Answer, Rep[U]) => Answer) = mapIntoAns(f, add)
//
//    def mapWithAnsIntoAns[U](f:(Rep[T], Answer) => (Answer, Rep[U])) = AGParser[U]  {
//      case (ans: Answer, input: Input) =>
//        this(ans, input) match {
//          case AGSuccess(result1, next1, ans1) =>
//            val res = f(result1, ans1)
//            AGSuccess(res._2, next1, res._1)
//          case AGFailure(msg1, next1) =>
//            AGFailure(msg1, next1)
//        }
//    }

//    def >>^^>>[U](f: (T, Answer) => (Answer, U)) = mapWithAnsIntoAns(f)

//    def flatMap[U](f: T => AGParser[U]) = AGParser[U] {
//      case (ans: Answer, input: Input) =>
//        this(ans, input) match {
//          case AGSuccess(result1, next1, ans1) =>
//            f(result1)(ans1, next1)
//          case AGFailure(msg1, next1) =>
//            AGFailure(msg1, next1)
//        }
//    }

//    def >>[U](f: T => AGParser[U]) = flatMap(f)
//    def into[U](f: T => AGParser[U]) = flatMap(f)

//    def flatMapWithAns[U](f: (T, Answer) => AGParser[U]) = AGParser[U] {
//      case (ans: Answer, input: Input) =>
//        this(ans, input) match {
//          case AGSuccess(result1, next1, ans1) =>
//            f(result1, ans1)(ans1, next1)
//          case AGFailure(msg1, next1) =>
//            AGFailure(msg1, next1)
//        }
//    }
//
//    def |[U >: T](that: => AGParser[U]) = AGParser[U] {
//      case (ans: Answer, input: Input) =>
//        this(ans, input) match {
//          case s:AGSuccess[U] => s //return result of first parser
//          case _ => that(ans, input) //or apply the second parser
//        }
//    }

    /*
     * sequence parsers return "~" tuples to allow easy pattern matching over the AGParseResults
     * in general only pipe original env through except for ~~
     */

    /**
     * Never augement the answer, always pipe the original answer
     * @param that
     * @tparam U
     * @return
     */
//    def ~[U](that: => AGParser[U]) = AGParser[~[T, U]] {
//      case (ans: Answer, input: Input) =>
//        this(ans, input) match {
//          case AGSuccess(result1, next1, _) =>
//            that(ans, next1) match { //use original Answer
//              case AGSuccess(result2, next2, _) =>
//                AGSuccess(new ~(result1, result2), next2, ans) //return original Answer
//              case AGFailure(msg2, next2) =>
//                AGFailure(msg2, next2)
//            }
//          case AGFailure(msg1, next1) =>
//            AGFailure(msg1, next1)
//        }
//    }
//
//    /**
//     * pipe Answer from parser1 to parser2 but not don't output as the final answer
//     * @param that
//     * @tparam U
//     * @return
//     */
//    def >>~[U](that: => AGParser[U]) = AGParser[~[T, U]] {
//      case (ans: Answer, input: Input) =>
//        this(ans, input) match {
//          case AGSuccess(result1, next1, ans1) =>
//            that(ans1, next1) match { //use augmented answer from parser1
//              case AGSuccess(result2, next2, _) =>
//                AGSuccess(new ~(result1, result2), next2, ans) //return initial answer
//              case AGFailure(msg2, next2) =>
//                AGFailure(msg2, next2)
//            }
//          case AGFailure(msg1, next1) =>
//            AGFailure(msg1, next1)
//        }
//    }
//
//    /**
//     * pipe Answer from parser2 to global environment of the combined parser
//     * @param that
//     * @tparam U
//     * @return
//     */
//    def ~>>[U](that: => AGParser[U]) = AGParser[~[T, U]] {
//      case (ans: Answer, input: Input) =>
//        this(ans, input) match {
//          case AGSuccess(result1, next1, _) =>
//            that(ans, next1) match { //use original answer
//              case AGSuccess(result2, next2, ans2) =>
//                AGSuccess(new ~(result1, result2), next2, ans2) //return answer from 2nd parser
//              case AGFailure(msg2, next2) =>
//                AGFailure(msg2, next2)
//            }
//          case AGFailure(msg1, next1) =>
//            AGFailure(msg1, next1)
//        }
//    }
//
//    /**
//     * pipe answer1 to parser2 and pipe answer2 to final result
//     * @param that
//     * @tparam U
//     * @return
//     */
//    def >>~>>[U](that: => AGParser[U]) = AGParser[~[T, U]] {
//      case (ans: Answer, input: Input) =>
//        this(ans, input) match {
//          case AGSuccess(result1, next1, ans1) =>
//            that(ans1, next1) match { //use new answer
//              case AGSuccess(result2, next2, ans2) =>
//                AGSuccess(new ~(result1, result2), next2, ans2) //return answer from 2nd parser
//              case AGFailure(msg2, next2) =>
//                AGFailure(msg2, next2)
//            }
//          case AGFailure(msg1, next1) =>
//            AGFailure(msg1, next1)
//        }
//    }
//
//    def ~>[U](that: => AGParser[U]) = AGParser[U] {
//      case (ans: Answer, input: Input) => this.~(that)(ans, input) match {
//        case AGSuccess(~(t: T, u: U), next, _) =>
//          AGSuccess[U](u, next, ans)
//        case AGFailure(msg1, next1) =>
//          AGFailure(msg1, next1)
//      }
//    }
//
//    def <~[U](that: => AGParser[U]) = AGParser[T] {
//      case (ans: Answer, input: Input) => this.~(that)(ans, input) match {
//        case AGSuccess(~(t: T, u: U), next, _) =>
//          AGSuccess[T](t, next, ans)
//        case AGFailure(msg1, next1) =>
//          AGFailure(msg1, next1)
//      }
//    }
//
//    def failureAG(s:String) = AGParser[T] {
//      case (ans: Answer, input: Input) =>
//        failure(s)(input) match {
//          case Success(result, next) => AGSuccess[T](result, next, ans)
//          case Failure(msg, next) => AGFailure(msg, next)
//        }
//    }


  }

  //help functions for Parsers trait

  def lift[T](pars: => Parser[T]) = AGParser[T] {
    case (ans: Answer, input: Input) =>
      val stag = pars(input)
      if (stag.isEmpty) AGFailure("futti", stag.next)
        else AGSuccess[T](stag.get, stag.next, ans)
  }
  //implicit def impLift(s:String) = lift(keyword(s))
  //implicit def impLift[T](p: Parser[T]) = lift(p)


//  def rep[T](pars: AGParser[T]):AGParser[List[T]] = AGParser[List[T]] {
//    case (ans: Answer, input: Input) =>
//      pars(ans, input) match {
//        case AGSuccess(result1, next1, ans1) =>
//          rep(pars)(ans, next1) match {
//            //pipe original Answer
//            case AGSuccess(result2, next2, ans2) => AGSuccess(result1 :: result2, next1, ans) //propagate old Answer
//            case fail => AGSuccess(result1 :: Nil, next1, ans)
//          }
//        case AGFailure(msg1, next1) => AGSuccess(Nil, next1, ans) //rep parser always suceeds
//      }
//  }
//
//  def repWithAns[T](pars: AGParser[T]):AGParser[List[T]] = AGParser[List[T]] {
//    case (ans: Answer, input: Input) =>
//      pars(ans, input) match {
//        case AGSuccess(result1, next1, ans1) =>
//          rep(pars)(ans1, next1) match {
//            //pipe original Answer
//            case AGSuccess(result2, next2, ans2) => AGSuccess(result1 :: result2, next1, ans2) //propagate old Answer
//            case fail => AGSuccess(result1 :: Nil, next1, ans)
//          }
//        case AGFailure(msg1, next1) => AGSuccess(Nil, next1, ans) //rep parser always suceeds
//      }
//  }

  object AGParser {
    //companion object to build new parsers without the need for and "val outer = this" reference
    def apply[V](f: (Answer, Input) => AGParseResult[V]) = new AGParser[V] {
      def apply(ans: Answer, input: Input) = f(ans, input)
    }
  }
}
