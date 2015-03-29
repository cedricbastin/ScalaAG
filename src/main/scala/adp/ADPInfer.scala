package adp

import agPC._
import agPC.TwoPhaseInferencer._
import scala.io.Source
//import scala.util.parsing.combinator.Parsers.Success
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/**
 * Created by cedricbastin on 24/03/15.
 */
object ADPInfer extends scala.App {
  trait Signature {
    type Alphabet // input type
    type Answer // output type
  }

  trait TypingSig extends Signature {
    //type Alphabet

    def tru: Answer
    def fals: Answer

    def num(s: String): Answer
    def succ(a: Answer): Answer
    def pred(a: Answer): Answer
    def iszero(a: Answer): Answer

    def iff(a1: Answer, a2: Answer, a3: Answer): Answer

    //def app(a:Answer, a1:Answer):Answer

    //def combine(a1: Answer, a2: Answer): Answer
  }

  trait TypingGrammar extends StandardTokenParsers with TypingSig {
    //    def Term: Parser[Term] = positioned(
    //      SimpleTerm ~ rep(SimpleTerm) ^^ { case t ~ ts => (t :: ts).reduceLeft[Term](App)}
    //        | failure("illegal start of term"))
    def Term(implicit a: Answer): Parser[Answer] =
      SimpleTerm ^^ (x => x) | {
        SimpleTerm >> (res => Term(res))
      } ^^ (x => x) | // => val combine(x, Term(x))} | //~ rep(SimpleTerm) ^^ { case t ~ ts => (t :: ts).reduceLeft[Term](App)}
        failure("illegal start of term")

    def SimpleTerm(implicit a: Answer): Parser[Answer] =
      "true" ^^^ tru |
        "false" ^^^ fals |
        numericLit ^^ {
          num(_)
        } |
        "succ" ~> Term ^^ {
          succ(_)
        } |
        "pred" ~> Term ^^ {
          pred(_)
        } |
        "iszero" ~> Term ^^ {
          iszero(_)
        } |
        "if" ~ Term ~ "then" ~ Term ~ "else" ~ Term ^^ {
          case "if" ~ t1 ~ "then" ~ t2 ~ "else" ~ t3 => iff(t1, t2, t3)
        }

    //        | ident ^^ (Var(_))
    //        | "\\" ~ ident ~ opt(":" ~ Type) ~ "." ~ Term ^^ {
    //        case "\\" ~ x ~ Some(":" ~ tp) ~ "." ~ t => Abs(x, tp, t)
    //        case "\\" ~ x ~ None ~ "." ~ t => Abs(x, EmptyType, t)
    //      }
    //        | "(" ~> Term <~ ")" ^^ { case t => t}
    //        | "let" ~ ident ~ "=" ~ Term ~ "in" ~ Term ^^ { case "let" ~ x ~ "=" ~ t1 ~ "in" ~ t2 => Let(x, t1, t2)}
    //        | failure("illegal start of simple term"))
  }

  //  def collect(env: Env, t: Term): TypingResult = t match {
  //    case True | False => TypingResult(TypeBool, noConstraints)
  //    case Zero => TypingResult(TypeNat, noConstraints)
  //
  //    case Var(x) =>
  //      val t1 : TypeScheme = lookup(env, x)
  //      if (t1 == null)
  //        throw TypeError("Unknown variable " + x)
  //      TypingResult(t1.instantiate, noConstraints)
  //
  //    case Succ(t1) =>
  //      val TypingResult(ty, const) = collect(env, t1)
  //      TypingResult(TypeNat, (ty, TypeNat) :: const)
  //
  //    case Pred(t1) =>
  //      val TypingResult(ty, const) = collect(env, t1)
  //      TypingResult(TypeNat, (ty, TypeNat) :: const)
  //
  //    case IsZero(t1) =>
  //      val TypingResult(ty, const) = collect(env, t1)
  //      TypingResult(TypeBool, (ty, TypeNat) :: const)
  //
  //    case If(cond, t1, t2) =>
  //      val TypingResult(tcond, const) = collect(env, cond)
  //      val TypingResult(ty1, const1) = collect(env, t1)
  //      val TypingResult(ty2, const2) = collect(env, t2)
  //      TypingResult(ty2, (tcond ,TypeBool)::(ty1, ty2)::const:::const1:::const2)
  //
  //    //case Abs(v:String, tp:TypeTree, t:tr) =>
  //    //env need to be calculated and given to the type inference of t!!!
  //
  //    case Abs(v: String, tp: TypeTree, t: Term) =>
  //      /**If the type for abs is not specified, we create a new TypeVar*/
  //      val tpsch = TypeScheme(Nil, if (tp != EmptyType) toType(tp) else Type.factorFresh)
  //      val TypingResult(ty, const) = collect((v,tpsch)::env, t)
  //      TypingResult(TypeFun(tpsch.tp, ty), const)
  //
  //    case App(t1: Term, t2: Term) => //TAPL: p.321
  //      /**Constraints for t1 + Type*/
  //      val TypingResult(ty1, const1) = collect(env, t1)
  //      /**Constraints for t2 + Type*/
  //      val TypingResult(ty2, const2) = collect(env, t2)
  //      val tx = Type.factorFresh
  //      TypingResult(tx, (ty1,TypeFun(ty2,tx))::const1:::const2)
  //
  //    case Let(x, v, t) =>
  //      val TypingResult(s, cstv) = collect(env, v)
  //      val subst = unify(cstv)
  //      val ty: Type = subst(s)
  //      val newenv = subst(env)
  //      val TypingResult(finaltp, cst2) = collect((x, generalize(newenv, ty)) :: newenv, t)
  //      TypingResult(finaltp, cstv:::cst2) //keep track of the constraints of the left-hand side!
  //  }

  trait TypingAlgebra extends TypingSig {
    type Env = List[(String, TypeScheme)]

    case class Answer(env: Env, tr: TypingResult)

    //type Answer = Ans

    type Constraint = (Type, Type)
    //(type Var, expected Type)
    val noConstraints: List[Constraint] = Nil
    //TypingResult(tpe, c)
    val tru =
      Answer(Nil, TypingResult(TypeBool, noConstraints))
    val fals =
      Answer(Nil, TypingResult(TypeBool, noConstraints))

    def num(s: String) =
      Answer(Nil, TypingResult(TypeNat, noConstraints))

    def pred(a: Answer) =
      Answer(a.env, TypingResult(TypeNat, (a.tr.tpe, TypeNat) :: a.tr.c))

    def succ(a: Answer) =
      Answer(a.env, TypingResult(TypeNat, (a.tr.tpe, TypeNat) :: a.tr.c))

    def iszero(a: Answer) =
      Answer(a.env, TypingResult(TypeBool, (a.tr.tpe, TypeNat) :: a.tr.c))

    def iff(cond: Answer, then: Answer, els: Answer) = {
      Answer(cond.env, TypingResult(els.tr.tpe, (cond.tr.tpe, TypeBool) ::(then.tr.tpe, els.tr.tpe) :: cond.tr.c ::: then.tr.c ::: els.tr.c))

    }

    class Clazz extends TypingGrammar with TypingAlgebra {
      val tests = Source.fromFile("test.in").getLines.filter(!_.startsWith("/*")).toList
      tests.foreach {
        testString => lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+")
          lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ", "pred", "iszero", "let", "in")
          val tokens = new lexical.Scanner(testString)
          print(testString + ": ")
          implicit val answer = Answer(Nil, TypingResult(TypeBool, noConstraints)) //base environment
          val parsed = SimpleTerm(answer)(tokens)
          parsed match {
            case Success(Answer(env, tr@TypingResult(tp, c)), _) => //
              try {
                val s = unify(c)
                println(s(tp))
              } catch {
                case tperror: Throwable => println(tperror.toString)
              }
            case e =>
              println(e)
          }
      }
    }
    val test = new Clazz
    println(test)
  }
}
