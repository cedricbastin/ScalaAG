package agParsers

import agPC.TypeScheme
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/**
 * Created by cedricbastin on 11/04/15.
 */

trait Signature2 {
  type Alphabet // input type
  type Answer // output type
}

object Nana extends App {
  trait Signature {
    //type Alphabet
    type Answer //top down, bottom up or both
    type Env
    type EnvElem
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

  class TypingParser extends StandardTokenParsers with AGParsers { //AGParser method override
    lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+")
    lexical.reserved ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ", "pred", "iszero", "let", "in")

  }

}
