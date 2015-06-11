package agParsers

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

trait AGSig {
  /**
   * Answer contains information which is piped around during parsing
   * It can contain the result of the current parsing
   * it can contain an environment of inherited attributes
   * it can contain an environment of synthecised attributed
   * the user can define those environments/ attributes as he likes
   */

  type AttrEnv //inherited attributes
  type Attr //synthezised attributes
  type AttrEnvLazy = AttrEnv => AttrEnv //lazily computed environment
  def combine(a1:AttrEnv, a2:AttrEnv):AttrEnv
}

/*
 * An partial implementation of a parser library to allow to compute semanic function over a parse tree at the same time as parsing
 */
trait AGParsers extends StandardTokenParsers with AGSig {
  /* AGSig defines abstract type members
   * "Attr" represent the Attributes potentially returned by the Parsers: AGParser[Attr]
   * "AttrEnv" represents the environment if inherited and syntezised attributes collected through the tree
   */

  sealed abstract class AGParseResult[+T] { // wrapper class for ParseResult[T] which cannot be extended
    def map[U](f:T => U) = this match {
      case AGSuccess(result, next, ans) => AGSuccess(f(result), next, ans)
      case AGFailure(msg, next, ans) => AGFailure(msg, next, ans)
    }
  }

  object AGParseResult { //companion object to help create AGParseResults
    def apply[T](pr:ParseResult[T], ans:AttrEnv) = pr match {
      case Success(result, next) => AGSuccess(result, next, ans)
      case NoSuccess(msg, next) => AGFailure(msg, next, ans) //matches both Error and Failure
    }
  }
  //TODO: still implement class NoSuccess extends ParseResult, Failure extends NoSuccess, Error extends NoSuccess
  case class AGSuccess[+T](result: T, next: Input, ans: AttrEnv) extends AGParseResult[T]
  case class AGFailure(msg: String, next: Input, ans: AttrEnv) extends AGParseResult[Nothing]

  // T is independent from abstract type "Attr" but could potentially be the same
  // parser combinator methods arguments need to be call by name otherwise the stack will overflow
  abstract class AGParser[+T] extends ((AttrEnv, Input) => AGParseResult[T]) {

    def validatePrim(p: T => Boolean) = AGParser[T] { //this method should be usable combined with map etc...
      case (ans, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            if (p(result1)) AGSuccess(result1, next1, ans)
            else AGFailure("validation failed on: "+result1+" ans: "+ans, next1, ans1)
          case AGFailure(msg1, next1, ans1) =>
            AGFailure(msg1, next1, ans1)
        }
    }

    //With the additional semantic knowledge we can create new validation to check for well-formedness
    //TODO: this method should be easily usable with "map" etc
    def validate(p: (T, AttrEnv) => Boolean) = AGParser[T] {
      case (ans, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            if (p(result1, ans1)) AGSuccess(result1, next1, ans1) //pipe this answer?
            else AGFailure("validation failed on: "+result1+" ans: "+ans, next1, ans1)
          case AGFailure(msg1, next1, ans1) =>
            AGFailure(msg1, next1, ans1)
        }
    }

    def map[U](f: T => U) = AGParser[U] {
      case (ans, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, _) =>
            AGSuccess(f(result1), next1, ans)
          case AGFailure(msg1, next1, ans1) =>
            AGFailure(msg1, next1, ans1)
        }
    }

    def ^^[U](f: T => U) = map(f)

    def ^^^[U](f: => U) = AGParser[U] {
      case (ans, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, _) =>
            AGSuccess(f, next1, ans)
          case AGFailure(msg1, next1, ans1) =>
            AGFailure(msg1, next1, ans1)
        }
    }

    def mapWithAns[U](f: (T, AttrEnv) => U) = AGParser[U] {
      case (ans, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            AGSuccess(f(result1, ans1), next1, ans)
          case AGFailure(msg1, next1, ans1) =>
            AGFailure(msg1, next1, ans1)
        }
    }
    def >>^^[U](f: (T, AttrEnv) => U) = mapWithAns(f)

    def mapIntoAns[U](f: T => (U, AttrEnv)) = AGParser[U] { //returned answer needs to be merged with previous one
      case (ans, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, _) => //should this new ans be used?
            val (res, ansx) = f(result1)
            AGSuccess(res, next1, combine(ans, ansx))
          case AGFailure(msg1, next1, ans1) =>
            AGFailure(msg1, next1, ans1)
        }
    }
    def ^^>>[U](f: T => (U, AttrEnv)) = mapIntoAns(f)

    def mapWithAnsIntoAns[U](f:(T, AttrEnv) => (U, AttrEnv)) = AGParser[U] {
      case (ans, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            val res = f(result1, ans1)
            AGSuccess(res._1, next1, res._2)
          case AGFailure(msg1, next1, ans1) =>
            AGFailure(msg1, next1, ans1)
        }
    }
    def >>^^>>[U](f: (T, AttrEnv) => (U, AttrEnv)) = mapWithAnsIntoAns(f)

    def flatMap[U](f: T => AGParser[U]) = AGParser[U] {
      case (ans, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            f(result1)(ans1, next1)
          case AGFailure(msg1, next1, ans1) =>
            AGFailure(msg1, next1, ans1)
        }
    }
    def >>[U](f: T => AGParser[U]) = flatMap(f)
    def into[U](f: T => AGParser[U]) = flatMap(f)

    def flatMapWithAns[U](f: (T, AttrEnv) => AGParser[U]) = AGParser[U] {
      case (ans, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            f(result1, ans1)(ans1, next1)
          case AGFailure(msg1, next1, ans1) =>
            AGFailure(msg1, next1, ans1)
        }
    }

    def |[U >: T](that: => AGParser[U]) = AGParser[U] {
      case (ans, input: Input) =>
        this(ans, input) match {
          case s:AGSuccess[U] => s //return result of first parser
          case _ => that(ans, input) //or apply the second parser
        }
    }

  /*
   * sequence parsers return "~" tuples to allow easy pattern matching over the AGParseResults
   * in general only original attribute environment is piped through the operator
   */
    def ~[U](that: => AGParser[U]) = AGParser[~[T, U]] {
      case (ans, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            that(ans, next1) match { //use original Answer
              case AGSuccess(result2, next2, ans2) =>
                AGSuccess(new ~(result1, result2), next2, ans) //return original Answer
              case AGFailure(msg2, next2, ans2) =>
                AGFailure(msg2, next2, ans2)
            }
          case AGFailure(msg1, next1, ans1) =>
            AGFailure(msg1, next1, ans1)
        }
    }

    /*
     * eg. parserC = parserA >>~ parserB
     * attribute environment is only piped from parserA to parserB won't be in the AGParseResult of parserC
     */
    def >>~[U](that: => AGParser[U]) = AGParser[~[T, U]] {
      case (ans, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            that(ans1, next1) match { //use augmented answer from parser1
              case AGSuccess(result2, next2, _) =>
                AGSuccess(new ~(result1, result2), next2, ans) //return initial answer
              case AGFailure(msg2, next2, ans2) =>
                AGFailure(msg2, next2, ans2)
            }
          case AGFailure(msg1, next1, ans1) =>
            AGFailure(msg1, next1, ans1)
        }
    }

    /*
     * eg. parserC = parserA ~>> parserB
     * the potentially updated attribute environment of parserA is not piped to parserB
     * however the attribute environment from parserB is included in the ParseResult of parserC
     */
    def ~>>[U](that: => AGParser[U]) = AGParser[~[T, U]] {
      case (ans, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, _) =>
            that(ans, next1) match { //use original answer
              case AGSuccess(result2, next2, ans2) =>
                AGSuccess(new ~(result1, result2), next2, ans2) //return answer from 2nd parser
              case AGFailure(msg2, next2, ans2) =>
                AGFailure(msg2, next2, ans2)
            }
          case AGFailure(msg1, next1, ans1) =>
            AGFailure(msg1, next1, ans1)
        }
    }

    def >>~>>[U](that: => AGParser[U]) = AGParser[~[T, U]] {
      case (ans, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            that(ans1, next1) match { //use new answer
              case AGSuccess(result2, next2, ans2) =>
                AGSuccess(new ~(result1, result2), next2, ans2) //return answer from 2nd parser
              case AGFailure(msg2, next2, ans2) =>
                AGFailure(msg2, next2, ans2)
            }
          case AGFailure(msg1, next1, ans1) =>
            AGFailure(msg1, next1, ans1)
        }
    }

    def ~>[U](that: => AGParser[U]) = AGParser[U] {
      case (ans, input: Input) => this.~(that)(ans, input) match {
        case AGSuccess(~(t, u), next, _) =>
          AGSuccess[U](u, next, ans)
        case AGFailure(msg1, next1, ans1) =>
          AGFailure(msg1, next1, ans1)
      }
    }

    def <~[U](that: => AGParser[U]) = AGParser[T] {
      case (ans, input: Input) => this.~(that)(ans, input) match {
        case AGSuccess(~(t, u), next, _) =>
          AGSuccess[T](t, next, ans)
        case AGFailure(msg1, next1, ans1) =>
          AGFailure(msg1, next1, ans1)
      }
    }

    def failureAG(s:String) = AGParser[T] {
      case (ans, input: Input) =>
        failure(s)(input) match {
          case Success(result, next) => AGSuccess[T](result, next, ans)
          case NoSuccess(msg, next) => AGFailure(msg, next, ans)
        }
    }
  }

  //easy interface to use the predefined parsers form The StandardToken library
  def lift[T](pars: => Parser[T]) = AGParser[T] {
    case (ans, input: Input) =>
      pars(input) match {
        case Success(result, next) => AGSuccess[T](result, next, ans) //pipe Answer through
        case NoSuccess(msg, next) => AGFailure(msg, next, ans)
      }
  }
  //implicit def impLift(s:String) = lift(keyword(s)) //this doesn't work well

  def rep[T](pars: AGParser[T]):AGParser[List[T]] = AGParser[List[T]] {
    case (ans, input: Input) =>
      pars(ans, input) match {
        case AGSuccess(result1, next1, _) =>
          rep(pars)(ans, next1) match {
            case AGSuccess(result2, next2, _) => AGSuccess(result1 :: result2, next2, ans)
            case fail => AGSuccess(result1 :: Nil, next1, ans)
          }
        case AGFailure(msg1, next1, _) => AGSuccess(Nil, next1, ans) //rep parser always suceeds worst case with Nil, don't propagate attribute env of failed parser
      }
  }

  //rep parser never fails!
  def repWithAns[T](pars: AGParser[T]):AGParser[List[T]] = AGParser[List[T]] {
    case (ans, input: Input) =>
      pars(ans, input) match {
        case AGSuccess(result1, next1, ans1) =>
          repWithAns(pars)(ans1, next1) match {
            //pipe original Answer
            case AGSuccess(result2, next2, ans2) => AGSuccess(result1 :: result2, next1, ans2)
            case fail => AGSuccess(result1 :: Nil, next1, ans1) //should never happen?
          }
        case AGFailure(msg1, next1, _) => AGSuccess(Nil, next1, ans)
      }
  }

  object AGParser {
    //companion object to build new parsers without the need for and "val outer = this" reference
    def apply[V](f: (AttrEnv, Input) => AGParseResult[V]) = new AGParser[V] {
      def apply(ans: AttrEnv, input: Input) = f(ans, input)
    }
  }
}
