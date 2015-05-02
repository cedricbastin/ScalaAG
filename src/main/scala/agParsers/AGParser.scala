package agParsers

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

//A parser library which allows to pipe an environment
trait AGParsers extends StandardTokenParsers with AGSig {
  // type Input = Reader[Elem] //Parsers.scala

  //Answer also contains environment
  abstract class AGParseResult[+T] //extends ParseResult[T] => SEALED CANNOT BE EXTENDED
  object AGParseResult {
    def apply[T](pr:ParseResult[T], ans:Answer) = pr match {
      case Success(result, next) => AGSuccess(result, next, ans)
      case Failure(msg, next) => AGFailure(msg, next)
    }
  }
  case class AGSuccess[+T](result: T, next: Input, ans: Answer) extends AGParseResult[T]
  case class AGFailure(msg: String, next: Input) extends AGParseResult[Nothing]

//  def addToEnv(p:AGParser[T]) = AGParser[U] {
//    //include Answer in the signature?
//    case (ans: Answer, input: Input) =>
//      p(ans, input) match {
//        case "\\" ~ x:String ~ ":" ~ tp:TypeTree =>
//      }
//  }

  //T is independent from Answer!
  abstract class AGParser[+T] extends ((Answer, Input) => AGParseResult[T]) {

    //Reader Monad is defined as a monad transformer:
    // (* -> *) -> * -> *
    // A -> E -> M A


    def mapWithEnv[U](f: (T, Answer) => U) = AGParser[U] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            AGSuccess(f(result1, ans), next1, ans1)
          case AGFailure(msg1, next1) =>
            AGFailure(msg1, next1)
        }
    }

    //FIXME: should mapping include answer treatment or not?
    def map[U](f: T => U) = AGParser[U] {
      //include Answer in the signature?
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            AGSuccess(f(result1), next1, ans1)
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

    def flatMapA[U](f: (T, Answer) => AGParser[U]) = AGParser[U] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            f(result1, ans1)(ans1, next1)
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

    //FIXME: we could also accept
    def |[U >: T](that: => AGParser[U]) = AGParser[U] {
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case s:AGSuccess[U] => s //return result of first parser
          case _ => that(ans, input) //or apply the second parser
        }
    }

    def ~[U](that: => AGParser[U]) = AGParser[~[T, U]] {
      //use the companion object instead of constructing new parser by hand
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          //the current parser
          case AGSuccess(result1, next1, ans1) =>
            that(ans, next1) match { //FIXME: use ans 1 or 2?
              //the following parser, piped ans "environment"
              case AGSuccess(result2, next2, ans2) =>
                AGSuccess(new ~(result1, result2), next2, ans) //FIXME: ans2 is piped "environment"
              case AGFailure(msg2, next2) =>
                AGFailure(msg2, next2)
            }
          case AGFailure(msg1, next1) =>
            AGFailure(msg1, next1)
        }
    }

    //"PIPE" environment through without collecting new one
    def ~~[U](that: => AGParser[U]) = AGParser[~[T, U]] {
      //use the companion object instead of constructing new parser by hand
      case (ans: Answer, input: Input) =>
        this(ans, input) match {
          case AGSuccess(result1, next1, ans1) =>
            that(ans, next1) match { //use initial anser / environment
              case AGSuccess(result2, next2, ans2) =>
                AGSuccess(new ~(result1, result2), next2, ans2) //FIXME: which ans to return?
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


  object AGParser {
    //companion object to build new parsers without the need for and "val outer = this" reference
    def apply[V](f: (Answer, Input) => AGParseResult[V]) = new AGParser[V] {
      def apply(ans: Answer, input: Input) = f(ans, input)
    }


    //for interability with simple parsers:
//    def apply[V](f: Input => ParseResult[V]) = new AGParser[V] {
//      def apply(ans: Answer, input: Input) = f(input) match {
//        case NoSuccess(msg, next) => AGFailure(msg, next)
//        case Success(res, next) => AGSuccess(res, next, ans) //FIXME: defaultAnswer
//      }
//    }
  }

  def lift[T](pars: Parser[T]) = AGParser[T] { //FIXME: by name: : => Parser[T]
    case (ans: Answer, input: Input) =>
      pars(input) match {
        case Success(result, next) => AGSuccess[T](result, next, ans) //
        case Failure(msg, next) => AGFailure(msg, next)
      }
  }
  //implicit def impLift(s:String) = lift(keyword(s))
  //implicit def impLift[T](p: Parser[T]) = lift(p)


  def rep[T](pars: AGParser[T]):AGParser[List[T]] = AGParser[List[T]] {
    case (ans: Answer, input: Input) =>
      if (input.atEnd)
        AGSuccess[List[T]](Nil, input, ans)
      else
        pars(ans, input) match {
        case AGSuccess(result1, next1, ans1) =>
          rep(pars)(ans, next1) match {
            case AGSuccess(result2:List[T], next2, ans2) => AGSuccess[List[T]](result1 :: result2, next1, ans1 ) //FIXME: propagate new ans?
            //should never happen:
            case x => x
          }
        case AGFailure(msg1, next1) => AGFailure(msg1, next1) //FIXME: is this a safe way to do this?
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
