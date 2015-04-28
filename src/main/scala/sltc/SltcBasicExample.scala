package sltc

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.io._

/**
 * Created by cedricbastin on 16/04/15.
 */
object SltcBasicExample extends App {
  class SltcParser extends StandardTokenParsers {
    lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+")
    lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ", "pred", "iszero", "let", "in")

    def Term: Parser[Term] = positioned(
      SimpleTerm ~ rep(SimpleTerm).? ^^ {
        case e1 ~ None => e1
        case e1 ~ Some(e2) => e2.foldLeft(e1)((a, b) => Application(a, b)) //left associative
      }
        | failure("illegal start of term"))


    def SimpleTerm: Parser[Term] = positioned(
      "true" ^^^ True
        | "false" ^^^ False
        | numericLit ^^ { case e2 => evalInt(e2.toInt)}
        | "pred" ~> Term ^^ { case e1 => Pred(e1)}
        | "succ" ~> Term ^^ { case e1 => Succ(e1)}
        | "iszero" ~> Term ^^ { case e1 => IsZero(e1)}
        | "if" ~> Term ~ ("then" ~> Term) ~ ("else" ~> Term) ^^ { case e1 ~ e2 ~ e3 => If(e1, e2, e3)}
        | ident ^^ { case e1 => Var(e1.toString)}
        | ("\\" ~> ident) ~ opt(":" ~> Type) ~ ("." ~> Term) ^^ {
          case e1 ~ Some(e2) ~ e3 => Abstraction(Var(e1.toString), e2, e3)
          case e1 ~ None ~ e3 => Abstraction(Var(e1.toString), EmptyType, e3)}
        //        | ("\\" ~> ident) ~ ("." ~> Term) ^^ { case e1 ~ e2 => Abstraction(Var(e1.toString), EmptyType, e2) }
        | "(" ~> Term <~ ")" ^^ { case e1 => e1}
        | failure("illegal start of simple term")
    )

    def SimpleType: Parser[Type] = positioned(
      "Bool" ^^^ BoolType
        | "Nat" ^^^ Nat
        | "(" ~> (Type <~ ")") ^^ { case t => t}
        | failure("illegal start of type"))

    def Type: Parser[Type] = positioned(
      SimpleType ~ ("->" ~> Type) ^^ { case e1 ~ e2 => Function(e1, e2)} //right associative
        | SimpleType ^^ { case e1 => e1}
    )

    def evalInt(i: Int): Term = {
      if (i == 0) {
        return Zero
      } else {
        return Succ(evalInt(i - 1))
      }
    }

    case class TypeError(pos: Position, msg: String) extends Exception(msg) {
      override def toString = {
        "Type Error: " + msg + "\n" + pos.longString
      }
    }

    type Context = List[(String, Type)]
    val initContext: List[(String, Type)] = Nil

    //recursive type inference method
    def typeof(ctx: Context, t: Term): Type = t match {
      case True | False => BoolType
      case Zero => Nat

      case Pred(t1) if (typeof(ctx, t1) == Nat) => Nat
      case Pred(t1) => throw new TypeError(t1.pos, "Nat type expected but " + typeof(ctx, t1) + " found")

      case Succ(t1) if (typeof(ctx, t1) == Nat) => Nat
      case Succ(t1) => throw new TypeError(t1.pos, "Nat type expected but " + typeof(ctx, t1) + " found")

      case IsZero(t1) if (typeof(ctx, t1) == Nat) => BoolType
      case IsZero(t1) => throw new TypeError(t1.pos, "Nat type expected but " + typeof(ctx, t1) + " found")

      case If(t1, t2, t3) if (typeof(ctx, t1) == BoolType && typeof(ctx, t2) == typeof(ctx, t3)) => typeof(ctx, t2)
      case If(t1, _, _) if (typeof(ctx, t1) != BoolType) => throw new TypeError(t.pos, "expected Bool, found " + typeof(ctx, t1))
      case _: If => throw new TypeError(t.pos, "type mismatch between conditional branches")

      case Var(id) =>
        ctx.filter(_._1 == id) match {
          case Nil => throw new TypeError(t.pos, "undefined variable \'" + id + "\'")
          case x :: xs => return x._2 //returns last added occurrence if ambiguous
        }

      case Abstraction(Var(x), ty, t2) => Function(ty, typeof((x, ty) :: ctx, t2))

      case Application(t1, t2) =>
        typeof(ctx, t1) match {
          case Function(td, ta) if (td == typeof(ctx, t2)) => ta
          case Function(td, ta) => throw new TypeError(t.pos, "parameter type mismatch: expected " + td + ", found " + typeof(ctx, t2))
          case _ => throw new TypeError(t.pos, "first argument of application is not an abstraction")
        }

      case _ => println(t); throw new TypeError(t.pos, "no type rule applies")
    }

    val tests = Source.fromFile("test.in").getLines.filter(!_.startsWith("/*")).toList
    tests.foreach {
      testString =>
        val tokens = new lexical.Scanner(testString)
        print(testString + " --> ")
        Term(tokens) match {
          case Success(x,e) =>
            try {
              println(typeof(initContext, x))
            } catch {case e => println(e)}
          case e => println(e)
        }
        println("")
    }
  }
  val x = new SltcParser()
}
