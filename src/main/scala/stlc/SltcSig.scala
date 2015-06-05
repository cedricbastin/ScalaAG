package stlc
import agParsers._

/**
 * Created by cedricbastin on 26/04/15.
 * A generic algebra which can be reused for different semantic implementations
 */
trait StlcSig extends AGSig {
  //type Ret //return type //answer is passed around implicitly
  def tru: Answer
  def fals: Answer

  def num(s: String): Answer
  def succ(a: Answer): Answer
  def pred(a: Answer): Answer
  def iszero(a: Answer): Answer

  def iff(a1: Answer, a2: Answer, a3: Answer): Answer

  def vari(s:String, a:Answer): Answer
  def absHead(ident:String, ty:Type): (Answer, Answer)
  def abs(tp:Answer, a:Answer): Answer
  def abs(ident:String, ty:Type, a:AnswerF): AnswerF //use this with AnswerF

  def app(a1: Answer, a2: Answer): Answer
}
