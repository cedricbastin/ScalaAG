package sltc

import agParsers._
import agParsers.AGParsers
import agPC.TwoPhaseInferencer //helper functions for TypingResults

/**
 * Created by cedricbastin on 25/04/15.
 */
/*
| is the alternation combinator. It says “succeed if either the left or right operand parse successfully”
~ is the sequential combinator. It says “succeed if the left operand parses successfully, and then the right parses successfully on the remaining input”
~> says “succeed if the left operand parses successfully followed by the right, but do not include the left content in the result”
<~ is the reverse, “succeed if the left operand is parsed successfully followed by the right, but do not include the right content in the result”
^^=> is the transformation combinator. It says “if the left operand parses successfully, transform the result using the function on the right”
rep => simply says “expect N-many repetitions of parser X” where X is the parser passed as an argument to rep
 */
object SltcAGParser extends App {

}


  trait StlcGrammar extends AGParsers with StlcSig {
    lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+")
    lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ", "pred", "iszero", "let", "in")

    def Term: AGParser[Answer] =
      SimpleTerm ^^ {
        x => x
      } | {
        SimpleTerm ~ Term ^^ { //environemnt is collected and piped!
          case a1 ~ a2 => app(a1, a2)
        }
        //(SimpleTerm >> (res => Term(res))) ^^ (x => x) //FIXME
        //SimpleTerm ~ rep(SimpleTerm) ^^ { case t ~ ts => (t :: ts).reduceLeft[Term](App)}
      } | // => val combine(x, Term(x))} | //
        lift(failure("illegal start of term"))

    //implicit def impLift(s:String) = lift(keyword(s))
    implicit def impLift[T](p:Parser[T]) = lift(p)


    case class AGSuccess[+T](result: T, next: Input, ans: Answer) extends AGParseResult[T]
    case class AGFailure(msg: String, next: Input) extends AGParseResult[Nothing]

//    def addEnv():AGParser[Answer] = AGParser[Answer] {
//      case (ans: Answer, input: Input) =>
//        ("\\" ~ ident ~ ":" ~ Type).flatMapA {
//          case ("\\" ~ x ~ ":" ~ tp, ans) =>
//            AGSuccess(tp, next, ans.add)
//          //flatmap only works on success values?
//      }
//    }

    def SimpleTerm: AGParser[Answer] = {
//      lift(keyword("true")) ~ lift(keyword("false")) ~ SimpleTerm  ^^ {
//        case dfsgfg ~ sdf ~ sdfd => tru
//      } |
        lift("true") ^^^ {
        tru
      } | lift("false") ^^^ {
        fals
      } | lift(numericLit) ^^ {
        num(_)
      } | lift("succ") ~> Term ^^ {
        succ(_)
      } | lift("pred") ~> Term ^^ {
        pred(_)
      } | lift("iszero") ~> Term ^^ {
        iszero(_)
      } | lift("if") ~ Term ~ lift("then") ~ Term ~ lift("else") ~ Term ^^ {
        case "if" ~ t1 ~ "then" ~ t2 ~ "else" ~ t3 => iff(t1, t2, t3)
//      } | lift(ident) ^^ {
//        x => vari (x, defAnswer) //FIXME: environment env
      } | lift("\\") ~ ident ~ lift(":") ~ Type ~ lift(".") ~ Term ^^ {
          case "\\" ~ x ~ ":" ~ tp ~ "." ~ term =>
            abs(x, tp.toType, term)
      } | lift("\\") ~ ident ~ lift(":") ~ Type.flatMap {
        case "\\" ~ x ~ ":" ~ tp =>
          //FIXME: access answer?
          (lift(".") ~> Term) ^^ {
            case a => abs(x, tp, a)}
      } | lift("(") ~> Term <~ lift(")") ^^ {
        case t => t
      } | lift(failure("illegal start of simple term"))

    def Type: Parser[TypeTree] = positioned(
      BaseType ~ opt("->" ~ Type) ^^ {
        case t1 ~ Some("->" ~ t2) => FunType(t1, t2)
        case t1 ~ None => t1
      } | failure("illegal start of type"))

    def BaseType: Parser[TypeTree] = positioned(
      "Bool" ^^^ BoolType
        | "Nat" ^^^ NatType
        | "(" ~> Type <~ ")" ^^ { case t => t}
    )
    }


//  trait ParsingAlgebra extends StlcSig {
//    type Answer = Term //no env needed
//    def tru = True
//    def fals = False
//
//    def num(s: String) = Infer.lit2Num(s.toInt)
//    def succ(a: Answer) = Succ(a)
//    def pred(a: Answer) = Pred(a)
//    def iszero(a: Answer) = IsZero(a)
//
//    def iff(a1: Answer, a2: Answer, a3: Answer) = If(a1, a2, a3)
//
//    def vari(s:String, a:Answer) = Var(s)
//    def abs(ident:String, ty:TypeScheme, a:Answer) = Abs2(ident, ty.tp, a)
//    def let(x: String, v: Answer, t: Answer) = Let(x, v, t)
//
//    def app(a1: Answer, a2: Answer) = App(a1, a2)
//  }


}
