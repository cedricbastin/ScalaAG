package stlc
import agParsers._

/**
 * Created by cedricbastin on 26/04/15.
 * A generic algebra which can be reused for different semantic implementations
 */
trait StlcSig extends AGSig {
  type SynAttr //return type //answer is passed around implicitly
  def tru: SynAttr
  def fals: SynAttr

  def num(s: String): SynAttr
  def succ(a: SynAttr): SynAttr
  def pred(a: SynAttr): SynAttr
  def iszero(a: SynAttr): SynAttr

  def iff(a1: SynAttr, a2: SynAttr, a3: SynAttr): SynAttr

  def vari(s:String, a:InAttrs): SynAttr
  def absHead(ident:String, ty:Type): (SynAttr, InAttrs)
  def abs(tp:SynAttr, a:SynAttr): SynAttr
  //def abs(ident:String, ty:Type, a:AnswerF): AnswerF //use this with AnswerF

  def app(a1: SynAttr, a2: SynAttr): SynAttr
}
