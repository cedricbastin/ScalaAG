package sltc

import agParsers.AGParsers //helper functions for TypingResults

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

trait StlcGrammar extends AGParsers with StlcSig {
  lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+")
  lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ", "pred", "iszero", "let", "in")

  def Term: AGParser[Answer] = {
    SimpleTerm ~ rep(SimpleTerm) ^^ {
      case a1 ~ a2 =>
        (a1 :: a2).reduceLeft[Answer](app)
    } | lift(failure("illegal start of term"))
  }

  //special case which need special handling
  def AbsHead: AGParser[Answer] = {
    lift("\\") ~ lift(ident) ~ lift(":") ~ TypePars ~ lift(".") ^^>>> {
      case "\\" ~ x ~ ":" ~ tp ~ "." => absHead(x, tp)
    }
  }

  def SimpleTerm: AGParser[Answer] = {
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
    } | lift(ident) >>^^ {
      case (x, a) => vari(x, a) //we need the environment to determine the type
    } | AbsHead ~>> Term ^^ { //pipe augmented answer with environment
      case head ~ term =>
        abs(head, term)
    } | lift("(") ~> Term <~ lift(")") ^^ {
      case t => t
    } | lift(failure("illegal start of simple term"))
  }

  def TypePars: AGParser[Type] = {
    BaseType ~ lift("->") ~ TypePars ^^  {
      case t1 ~ "->" ~ t2 => TypeFun(t1, t2) //right associative, no need for left recursion
    } | BaseType ^^ { //order of parser combinators is important such that
      x => x
    } | lift(failure("illegal start of type"))
  }

  def BaseType: AGParser[Type] = {
    lift("Bool") ^^^ {
      TypeBool
    } | lift("Nat") ^^^ {
      TypeNat
    } | lift("(") ~> TypePars <~ lift(")") ^^ {
      case x => x
    }
  }
}
