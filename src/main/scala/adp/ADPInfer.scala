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
    //type Alphabet
    type Answer //top down, bottom up or both
    def addToEnv[E](a:Answer, e:E): Answer
  }

  trait StlcSig extends Signature {
    def tru: Answer
    def fals: Answer

    def num(s: String): Answer
    def succ(a: Answer): Answer
    def pred(a: Answer): Answer
    def iszero(a: Answer): Answer

    def iff(a1: Answer, a2: Answer, a3: Answer): Answer

    def vari(s:String, a:Answer): Answer
    def abs(ident:String, ty:TypeScheme, a:Answer): Answer //XXX: TypeTree
    def let(x: String, v: Answer, t: Answer): Answer //for let polymorphism

    def app(a1: Answer, a2: Answer): Answer
  }

  trait TypingGrammar extends StandardTokenParsers with StlcSig {
    lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+")
    lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ", "pred", "iszero", "let", "in")

    def Term(implicit a: Answer): Parser[Answer] =
      SimpleTerm ^^ (x => x) | {
        SimpleTerm flatMap {case a1 => Term(a1) map {case ans => app(a1, ans)} } //app foldleft -> env not really needed here!
        //(SimpleTerm >> (res => Term(res))) ^^ (x => x) //FIXME
      } | // => val combine(x, Term(x))} | //~ rep(SimpleTerm) ^^ { case t ~ ts => (t :: ts).reduceLeft[Term](App)}
        failure("illegal start of term")

    def SimpleTerm(implicit a: Answer): Parser[Answer] =
      "true" ^^^ {
        tru
      } | "false" ^^^ {
        fals
      } | numericLit ^^ {
        num(_)
      } | "succ" ~> Term ^^ {
        succ(_)
      } | "pred" ~> Term ^^ {
        pred(_)
      } | "iszero" ~> Term ^^ {
        iszero(_)
      } | "if" ~ Term ~ "then" ~ Term ~ "else" ~ Term ^^ {
        case "if" ~ t1 ~ "then" ~ t2 ~ "else" ~ t3 => iff(t1, t2, t3)
      } | ident ^^ {x =>
        vari(x, a)
      } | ("\\" ~ ident ~ opt(":" ~ Type)).flatMap {
        case "\\" ~ x ~ Some(":" ~ tp) =>
          val ts = TypeScheme(Nil, if (tp != EmptyType) toType(tp) else agPC.Type.factorFresh) //always nonempty?
          implicit val newA = addToEnv(a, (x, ts))
          ("." ~> Term(newA)) ^^ { case a => abs(x, ts, a)}
        case "\\" ~ x ~ None =>
          val ts = TypeScheme(Nil, agPC.Type.factorFresh)
          implicit val newA = addToEnv(a, (x, ts))
          ("." ~> Term(newA)) ^^ { case a => abs(x, ts, a)}
      } | ("let" ~ ident ~ "=" ~ Term ~ "in").flatMap {
        case "let" ~ x ~ "=" ~ t1 ~ "in" =>
          //val subst = unify(t1.asInstanceOf[Tuple])
          Term ^^ {case t2 => let(x, t1, t2)}
      } | "(" ~> Term <~ ")" ^^ {
        case t => t
      } | failure("illegal start of simple term")

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

  trait ParsingAlgebra extends StlcSig {
    type Answer = Term //no env needed
    def tru = True
    def fals = False

    def num(s: String) = Infer.lit2Num(s.toInt)
    def succ(a: Answer) = Succ(a)
    def pred(a: Answer) = Pred(a)
    def iszero(a: Answer) = IsZero(a)

    def iff(a1: Answer, a2: Answer, a3: Answer) = If(a1, a2, a3)

    def vari(s:String, a:Answer) = Var(s)
    def abs(ident:String, ty:TypeScheme, a:Answer) = Abs2(ident, ty.tp, a)
    def let(x: String, v: Answer, t: Answer) = Let(x, v, t)

    def app(a1: Answer, a2: Answer) = App(a1, a2)
  }

  trait TypingAlgebra extends StlcSig { //collect reformulated
    type EnvElem = (String, TypeScheme)
    type Env = List[EnvElem]
    type Constraint = (Type, Type)

    //env = top down
    //tr = bottom up
    case class Answer(env: Env, tr: TypingResult)

    val noEnv = Nil
    val noConstraints: List[Constraint] = Nil
    val defAns = Answer(Nil, TypingResult(TypeBool, noConstraints))

    val tru =
      Answer(noEnv, TypingResult(TypeBool, noConstraints))
    val fals =
      Answer(noEnv, TypingResult(TypeBool, noConstraints))

    def vari(s:String, a:Answer) = {
      val t1: TypeScheme = lookup(a.env, s)
      if (t1 == null)
        throw TypeError("Unknown variable " + s)
      Answer(a.env, TypingResult(t1.instantiate, noConstraints))
    }

    def num(s: String) = //parsed correctly as a number
      Answer(Nil, TypingResult(TypeNat, noConstraints))

    def succ(a: Answer) =
      Answer(a.env, TypingResult(TypeNat, (a.tr.tpe, TypeNat) :: a.tr.c))

    def pred(a: Answer) =
      Answer(a.env, TypingResult(TypeNat, (a.tr.tpe, TypeNat) :: a.tr.c))

    def iszero(a: Answer) =
      Answer(a.env, TypingResult(TypeBool, (a.tr.tpe, TypeNat) :: a.tr.c))

    def iff(cond: Answer, then: Answer, els: Answer) =
      Answer(cond.env, TypingResult(els.tr.tpe, (cond.tr.tpe, TypeBool) :: (then.tr.tpe, els.tr.tpe) :: cond.tr.c ::: then.tr.c ::: els.tr.c))
    //FIXME noEnv?

    def abs(v: String, ts: TypeScheme, a:Answer) = {
      Answer(a.env, TypingResult(TypeFun(ts.tp, a.tr.tpe), a.tr.c))
    }

    def app(a1: Answer, a2: Answer) = defAns//TAPL: p.321
    /**Constraints for t1 + Type*/
//    val TypingResult(ty1, const1) = collect(env, t1)
//    /**Constraints for t2 + Type*/
//    val TypingResult(ty2, const2) = collect(env, t2)
//    val tx = Type.factorFresh
//    TypingResult(tx, (ty1,TypeFun(ty2,tx))::const1:::const2)

    def let(x: String, v: Answer, t: Answer) = defAns
//    val TypingResult(s, cstv) = collect(env, v)
//    val subst = unify(cstv)
//    val ty: Type = subst(s)
//    val newenv = subst(env)
//    val TypingResult(finaltp, cst2) = collect((x, generalize(newenv, ty)) :: newenv, t)
//    TypingResult(finaltp, cstv:::cst2) //keep track of the constraints of the left-hand side!
  }

  class ParsingTest extends TypingGrammar with ParsingAlgebra {
    def addToEnv[Int](a:Answer, i:Int) = a //should never be used!
    val tests = Source.fromFile("test.in").getLines.filter(!_.startsWith("/*")).toList
    tests.foreach {
      testString =>
        val tokens = new lexical.Scanner(testString)
        print(testString + " --> ")
        implicit val answer:Answer = agPC.False //base environment and acc
      val parsed = Term(answer)(tokens)
        println(parsed)
//        parsed match {
//          case Success(term) => println(term)
//          case e => println(e)
//        }
    }
  }
    class TypingTest extends TypingGrammar with TypingAlgebra {
      def addToEnv[EnvElem](a:Answer, e:EnvElem) = Answer((e :: a.env.toList).asInstanceOf[Env], a.tr) //FIXME: env hack

      val tests = Source.fromFile("test.in").getLines.filter(!_.startsWith("/*")).toList
      tests.foreach {
        testString =>
          val tokens = new lexical.Scanner(testString)
          print(testString + " --> ")
          implicit val answer = Answer(Nil, TypingResult(TypeBool, noConstraints)) //base environment
          val parsed = Term(answer)(tokens)
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
  println("PARSING:")
  val parsingTest = new ParsingTest()
  println()
  println("TYPING:")
  val typingResult = new TypingTest()
}
