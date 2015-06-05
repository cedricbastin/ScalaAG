package stlc
import agParsers._

/**
 * Created by cedricbastin on 26/04/15.
 * A generic algebra which can be reused for different semantic implementations
 */
trait StlcSig extends AGSig {
  type Ret //return type //answer is passed around implicitly
  def tru: Ret
  def fals: Ret

  def num(s: String): Ret
  def succ(a: Ret): Ret
  def pred(a: Ret): Ret
  def iszero(a: Ret): Ret

  def iff(a1: Ret, a2: Ret, a3: Ret): Ret

  def vari(s:String, a:Answer): Ret
  def absHead(ident:String, ty:Type): (Ret, Answer)
  def abs(tp:Ret, a:Ret): Ret
  //def abs(ident:String, ty:Type, a:AnswerF): AnswerF //use this with AnswerF

  def app(a1: Ret, a2: Ret): Ret
}
