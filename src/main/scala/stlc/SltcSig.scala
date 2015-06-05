package stlc
import agParsers._

/**
 * Created by cedricbastin on 26/04/15.
 * A generic algebra which can be reused for different semantic implementations
 */
trait StlcSig extends AGSig {
  type Attr //return type //answer is passed around implicitly
  def tru: Attr
  def fals: Attr

  def num(s: String): Attr
  def succ(a: Attr): Attr
  def pred(a: Attr): Attr
  def iszero(a: Attr): Attr

  def iff(a1: Attr, a2: Attr, a3: Attr): Attr

  def vari(s:String, a:AttrEnv): Attr
  def absHead(ident:String, ty:Type): (Attr, AttrEnv)
  def abs(tp:Attr, a:Attr): Attr
  //def abs(ident:String, ty:Type, a:AnswerF): AnswerF //use this with AnswerF

  def app(a1: Attr, a2: Attr): Attr
}
