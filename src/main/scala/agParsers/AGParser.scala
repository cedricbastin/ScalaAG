package agParsers

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.virtualization.lms.common._

trait AGSig {
  /**
   * Answer contains information which is piped around during parsing
   * It can contain the result of the current parsing
   * it can contain an environment of inherited attributes
   * it can contain an environment of synthecised attributed
   * the user can define those environments/ attributes as he likes
   */

  type Answer
  type AnswerF = Answer => Answer //lazily computed environment
  def combine(a1:Answer, a2:Answer):Answer
}

/*
 * An partial implementation of a parser library to allow to compute semanic function over a parse tree at the same time as parsing
 */
trait AGParsers extends StandardTokenParsers with AGSig {

  // type Answer is an abstract data type which can be configures to contain differnent environments needed to execute the semantic functions
  // is used instead of carrying around an additional type parameter

  sealed abstract class AGParseResult[+T] // wrapper class for ParseResult[T] which cannot be extended
  object AGParseResult {
    def apply[T](pr:ParseResult[T], ans:Answer) = pr match {
      case Success(result, next) => AGSuccess(result, next, ans)
      case Failure(msg, next) => AGFailure(msg, next)
    }
  }
  //TODO: class NoSuccess extends ParseResult, Failure extends NoSuccess, Error extends NoSuccess
  case class AGSuccess[+T](result: T, next: Input, ans: Answer) extends AGParseResult[T]
  case class AGFailure(msg: String, next: Input) extends AGParseResult[Nothing]

  //T is independent from Answer but can be included as additional information in an Answer if needed
  // parser combinator methods arguments need to be call by name otherwise the stack will overflow
  abstract class AGParser[+T] extends ((Answer, Input) => AGParseResult[T]) {

    def map[U](f: T => U) = AGParser[U] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            AGSuccess(f(result1), next1, ans1)
          case AGFailure(msg1, next1) =>
            AGFailure(msg1, next1)
        }
    }

    def ^^[U](f: T => U) = map(f)

    def ^^^[U](f: => U) = AGParser[U] {
      //include Answer in the signature?
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            AGSuccess(f, next1, ans1)
          case AGFailure(msg1, next1) =>
            AGFailure(msg1, next1)
        }
    }

    def mapWithAns[U](f: (T, Answer) => U) = AGParser[U] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            AGSuccess(f(result1, ans1), next1, ans)
          case AGFailure(msg1, next1) =>
            AGFailure(msg1, next1)
        }
    }

    def >>^^[U](f: (T, Answer) => U) = mapWithAns(f)

    def mapIntoAns[U](f: T => U)(add:(Answer, U) => Answer) = AGParser[U] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            val res = f(result1)
            AGSuccess(res, next1, add(ans, res))
          case AGFailure(msg1, next1) =>
            AGFailure(msg1, next1)
        }
    }

    def ^^>>[U](f: T => U)(add:(Answer, U) => Answer) = mapIntoAns(f)(add)

    def ^^>>>(f: T => Answer) = AGParser[Answer] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            val res = f(result1)
            AGSuccess(res, next1, combine(res, ans))
          case AGFailure(msg1, next1) =>
            AGFailure(msg1, next1)
        }
    }

    def flatMap[U](f: T => AGParser[U]) = AGParser[U] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            f(result1)(ans1, next1)
          case AGFailure(msg1, next1) =>
            AGFailure(msg1, next1)
        }
    }

    def >>[U](f: T => AGParser[U]) = flatMap(f)

    def flatMapWithAns[U](f: (T, Answer) => AGParser[U]) = AGParser[U] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            f(result1, ans1)(ans1, next1)
          case AGFailure(msg1, next1) =>
            AGFailure(msg1, next1)
        }
    }

    def |[U >: T](that: => AGParser[U]) = AGParser[U] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case s:AGSuccess[U] => s //return result of first parser
          case _ => that(ans, input) //or apply the second parser
        }
    }

  /*
   * sequence parsers return "~" tuples to allow easy pattern matching over the AGParseResults
   * in general only pipe original env through except for ~~
   */

    def ~[U](that: => AGParser[U]) = AGParser[~[T, U]] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            that(ans, next1) match { //use original Answer
              case AGSuccess(result2, next2, ans2) =>
                AGSuccess(new ~(result1, result2), next2, ans)
              case AGFailure(msg2, next2) =>
                AGFailure(msg2, next2)
            }
          case AGFailure(msg1, next1) =>
            AGFailure(msg1, next1)
        }
    }

    //augment Answer with new Environment
    def ~>>[U](that: => AGParser[U]) = AGParser[~[T, U]] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            that(ans1, next1) match { //use initial anser / environment
              case AGSuccess(result2, next2, ans2) =>
                AGSuccess(new ~(result1, result2), next2, ans)
              case AGFailure(msg2, next2) =>
                AGFailure(msg2, next2)
            }
          case AGFailure(msg1, next1) =>
            AGFailure(msg1, next1)
        }
    }

    def ~>[U](that: => AGParser[U]) = AGParser[U] {
      case (ans: Answer, input: Input) => this.~(that)(ans, input) match {
        case AGSuccess(~(t: T, u: U), next, ans) =>
          AGSuccess[U](u, next, ans)
        case AGFailure(msg1, next1) =>
          AGFailure(msg1, next1)
      }
    }

    def <~[U](that: => AGParser[U]) = AGParser[T] {
      case (ans: Answer, input: Input) => this.~(that)(ans, input) match {
        case AGSuccess(~(t: T, u: U), next, ans) =>
          AGSuccess[T](t, next, ans)
        case AGFailure(msg1, next1) =>
          AGFailure(msg1, next1)
      }
    }

    def failureAG(s:String) = AGParser[T] {
      case (ans: Answer, input: Input) =>
        failure(s)(input) match {
          case Success(result, next) => AGSuccess[T](result, next, ans)
          case Failure(msg, next) => AGFailure(msg, next)
        }
    }


  }

  //help functions for Parsers trait

  def lift[T](pars: => Parser[T]) = AGParser[T] {
    case (ans: Answer, input: Input) =>
      pars(input) match {
        case Success(result, next) => AGSuccess[T](result, next, ans) //pipe Answer through
        case Failure(msg, next) => AGFailure(msg, next)
      }
  }
  //implicit def impLift(s:String) = lift(keyword(s))
  //implicit def impLift[T](p: Parser[T]) = lift(p)


  def rep[T](pars: AGParser[T]):AGParser[List[T]] = AGParser[List[T]] {
    case (ans: Answer, input: Input) =>
      pars(ans, input) match {
        case AGSuccess(result1, next1, ans1) =>
          rep(pars)(ans, next1) match {
            //pipe original Answer
            case AGSuccess(result2, next2, ans2) => AGSuccess(result1 :: result2, next1, ans) //propagate old Answer
            case fail => AGSuccess(result1 :: Nil, next1, ans)
          }
        case AGFailure(msg1, next1) => AGSuccess(Nil, next1, ans) //rep parser always suceeds
      }
  }

  def repWithAns[T](pars: AGParser[T]):AGParser[List[T]] = AGParser[List[T]] {
    case (ans: Answer, input: Input) =>
      pars(ans, input) match {
        case AGSuccess(result1, next1, ans1) =>
          rep(pars)(ans1, next1) match {
            //pipe original Answer
            case AGSuccess(result2, next2, ans2) => AGSuccess(result1 :: result2, next1, ans2) //propagate old Answer
            case fail => AGSuccess(result1 :: Nil, next1, ans)
          }
        case AGFailure(msg1, next1) => AGSuccess(Nil, next1, ans) //rep parser always suceeds
      }
  }

  object AGParser {
    //companion object to build new parsers without the need for and "val outer = this" reference
    def apply[V](f: (Answer, Input) => AGParseResult[V]) = new AGParser[V] {
      def apply(ans: Answer, input: Input) = f(ans, input)
    }
  }
}
