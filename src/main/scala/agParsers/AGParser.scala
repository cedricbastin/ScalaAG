package agParsers

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

trait Signature {
  //type Alphabet
  type Answer //contains environment somehow! we don't want to add another type parameter to AGParser which would also only give limited functionality
  def defaultAnswer: Answer
  //could contain something like Answer(inheritedEnv, Value, synthezisedEnv)
  type TopEnv
  type TopEnvElem
  type BotEnv
  type BotEnvElem
  //def addToEnv[E](a:Answer, e:E): Answer
}

//A parser library which allows to pipe an environment
trait AGParsers extends StandardTokenParsers with Signature {
  // type Input = Reader[Elem] //Parsers.scala

  //can a trait extend a class???
  abstract class AGParseResult[T] //extends ParseResult; SEALED CANNOT BE EXTENDED
  case class AGFailure[T](msg: String, next: Input) //extends AGParseResult[T]
  case class AGSuccess[+T](result: T, next: Input, ans: Answer) //extends AGParseResult[T]


  implicit def toAGParser(parser:Parser[T]) = AGPArser[T] {

  }
  //T is independent from Answer!
  abstract class AGParser[T] extends ((Answer, Input) => AGParseResult[T]) {


    def map[U](f: T => U) = AGParser[U] {
      //include Answer in the signature?
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case Success(result, next, ans) =>
            AGSuccess(f(result), next, ans) //FIXME: utiliser ans dans f?
          case Failure(msg, next) =>
            AGFailure[U](msg, next)
        }
    }

    def flatMap[U](f: T => AGParser[U]) = AGParser[U] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result, next, ans) =>
            f(result)(ans, next)
          case Failure(msg, next) =>
            AGFailure[U](msg, next)
        }
    }

    def ^^[U](f: T => U) = map(f)

    def ^^^[U](f: => U) = AGParser[U] {
      //include Answer in the signature?
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result, next, ans) =>
            AGSuccess(f, next, ans)
          case Failure(msg, next) =>
            AGFailure[U](msg, next)
        }
    }

    def ~[U](that: AGParser[U]) = AGParser[(T, U)] {
      //use the companion object instead of constructing new parser by hand
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          //the current parser
          case AGSuccess(result1, next1, ans1) =>
            that(ans1, next1) match {
              //the following parser, piped ans "environment"
              case AGSuccess(result2, next2, ans2) =>
                Success((result1, result2), next2, ans2) //ans2 is piped "environment"
              case AGFailure(msg2, next2) =>
                AGFailure[(T, U)](msg2, next2)
            }
          case AGFailure(msg1, next1) =>
            AGFailure[(T, U)](msg1, next1)
        }
    }

    def ~>[U](that: AGParser[U]) = {
      this.~(that) match {
        case AGSuccess((t: T, u: U), next, ans) =>
          AGSuccess[U](u, next, ans)
        case Failure(msg1, next1) =>
          Failure[U](msg1, next1)
      }
    }

    def <~[U](that: AGParser[U]) = {
      this.~(that) match {
        case AGSuccess((t: T, u: U), next, ans) =>
          AGSuccess[T](t, next, ans)
        case AGFailure(msg1, next1) =>
          AGFailure[T](msg1, next1)
      }
    }

    def StringParser(s:String) = AGParser[T] {

    }
  }

  object AGParser extends Parser {
    //companion object to build new parsers without the need for and "val outer = this" reference
    def apply[V](f: (Answer, Input) => AGParseResult[V]) = new AGParser[V] {
      def apply(ans: Answer, input: Input) = f(ans, input)
    }
    //for interability with the string parsers:
    def apply[V](f: Input => ParseResult[V]) = new AGParser[V] {
      def apply(ans: Answer, input: Input) = f(input) match {
        case NoSuccess(msg, next) => AGFailure(msg, next)
        case Success(res, next) => AGSuccess(res, next, ans) //FIXME: defaultAnswer
      }
    }
  }
}


//trait StagedParsers
//        extends ParseResultOps
//        with OptionOps
//        with ReaderOps
//        with MyTupleOps
//        with IfThenElse{
//
//abstract class Parser[+T:Manifest]
//        extends(Rep[Input]=>Rep[ParseResult[T]]){
//
///**
// * The flatMap operation
// */
//private def flatMap[U:Manifest](f:Rep[T]=>Parser[U])=Parser[U]{input=>
//        val tmp=this(input)
//        if(tmp.isEmpty)Failure[U](input)
//        else{
//        val x=f(tmp.get).apply(tmp.next)
//        if(x.isEmpty)Failure[U](input)else x
//        }
//        }
//
//        def>>[U:Manifest](f:Rep[T]=>Parser[U])=flatMap(f)
//
//        /**
//         * The concat operation
//         * implementing with `flatMap` produces worse code
//         * TODO: check the reason
//         */
//        def~[U:Manifest](that:Parser[U])=Parser[(T,U)]{input=>
//        val x=this(input)
//        if(x.isEmpty)Failure[(T,U)](input)
//        else{
//        val y=that(x.next)
//        if(y.isEmpty)Failure[(T,U)](input)
//        else Success(make_tuple2(x.get,y.get),y.next)
//        }
//        }
//
//        /**
//         * get right hand side result
//         */
//        def~>[U:Manifest](that:=>Parser[U])=Parser[U]{input=>
//        val x=this(input)
//        if(x.isEmpty)Failure[U](input)else that(x.next)
//        }
//
//        /**
//         * get left hand side result
//         */
//        def<~[U:Manifest](that:=>Parser[U])=Parser[T]{input=>
//        val x=this(input)
//
//        if(x.isEmpty)x
//        else{
//        val y=that(x.next)
//        if(y.isEmpty)Failure[T](input)else Success(x.get,y.next)
//        }
//        }
//
//        /**
//         * The map operation
//         */
//        def map[U:Manifest](f:Rep[T]=>Rep[U])=Parser[U]{input=>
//        this(input)map f
//        }
//
//        }
//
//        /**
//         * a 'conditional' parser
//         * lifts conditional expressions to parser level
//         */
//        def __ifThenElse[A:Manifest](
//        cond:Rep[Boolean],
//        thenp:=>Parser[A],
//        elsep:=>Parser[A]
//        ):Parser[A]=Parser[A]{input=>if(cond)thenp(input)else elsep(input)}
//
//        /**
//         * companion object for apply function
//         */
//        object Parser{
//        def apply[T:Manifest](f:Rep[Input]=>Rep[ParseResult[T]])=new Parser[T]{
//        def apply(in:Rep[Input])=f(in)
//        }
