package agPC

import scala.io.Source
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/** This object implements a parser and evaluator for the
  * simply typed lambda calculus found in Chapter 9 of
  * the TAPL book.
  */
object Infer extends StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+")
  lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
    "pred", "iszero", "let", "in")

  /** <pre>
    * Term     ::= SimpleTerm { SimpleTerm }</pre>
    */
  def Term: Parser[Term] = positioned(
    SimpleTerm ~ rep(SimpleTerm) ^^ { case t ~ ts => (t :: ts).reduceLeft[Term](App)}
      | failure("illegal start of term"))


  /** <pre>
    * SimpleTerm ::= "true"
    * | "false"
    * | number
    * | "succ" Term
    * | "pred" Term
    * | "iszero" Term
    * | "if" Term "then" Term "else" Term
    * | ident
    * | "\" ident [":" Type] "." Term
    * | "(" Term ")"
    * | "let" ident [":" Type] "=" Term "in" Term</pre>
    */
  def SimpleTerm: Parser[Term] = positioned(
    "true" ^^^ True
      | "false" ^^^ False
      | numericLit ^^ (chars => lit2Num(chars.toInt))
      | "succ" ~ Term ^^ { case "succ" ~ t => Succ(t)}
      | "pred" ~ Term ^^ { case "pred" ~ t => Pred(t)}
      | "iszero" ~ Term ^^ { case "iszero" ~ t => IsZero(t)}
      | "if" ~ Term ~ "then" ~ Term ~ "else" ~ Term ^^ {
      case "if" ~ t1 ~ "then" ~ t2 ~ "else" ~ t3 => If(t1, t2, t3)
    }
      | ident ^^ (Var(_))
      | "\\" ~ ident ~ opt(":" ~ Type) ~ "." ~ Term ^^ {
      case "\\" ~ x ~ Some(":" ~ tp) ~ "." ~ t => Abs(x, tp, t)
      case "\\" ~ x ~ None ~ "." ~ t => Abs(x, EmptyType, t)
    }
      | "(" ~> Term <~ ")" ^^ { case t => t}
      | "let" ~ ident ~ "=" ~ Term ~ "in" ~ Term ^^ { case "let" ~ x ~ "=" ~ t1 ~ "in" ~ t2 => Let(x, t1, t2)}
      | failure("illegal start of simple term"))

  /** <pre>
    * Type       ::= SimpleType { "->" Type }</pre>
    */
  def Type: Parser[TypeTree] = positioned(
    BaseType ~ opt("->" ~ Type) ^^ {
      case t1 ~ Some("->" ~ t2) => FunType(t1, t2)
      case t1 ~ None => t1
    }
      | failure("illegal start of type"))

  /** <pre>
    * BaseType ::= "Bool" | "Nat" | "(" Type ")"</pre>
    */
  def BaseType: Parser[TypeTree] = positioned(
    "Bool" ^^^ BoolType
      | "Nat" ^^^ NatType
      | "(" ~> Type <~ ")" ^^ { case t => t}
  )

  def lit2Num(n: Int): Term =
    if (n == 0) Zero else Succ(lit2Num(n - 1))

  val Inferencer = TwoPhaseInferencer

  //def Term: Parser[Term] typeOf(t: Term): Type // map: Term -> Term
  //def Term: Parser[Type] //map Type -> Type => rewrite typeOf
  //def Term: Parser[(Term, Type)] //access
  //def Term
  def main(args: Array[String]) {
    val tests = Source.fromFile("test.in").getLines.filter(!_.startsWith("/*")).toList
    tests.foreach { testString =>
      test1(testString)
    }
    //test
  }
  def test1(testString:String) = {
    val tokens = new lexical.Scanner(testString)
    //val phrased = phrase(Term)(token) //why would this be better?
    def id[T](x: T) = x
    def idTerm(x: Term) = x
    val idt = idTerm _
    val parsed = Term(tokens)
    parsed match {
      case Success(trees, _) =>
        try {
          println(s"tree: $trees")
          println(s"typed: ${Inferencer.typeOf(trees)}")
        } catch {
          case tperror: Throwable => println(tperror.toString)
        }
      case e =>
        println(e)
    }
  }
}
