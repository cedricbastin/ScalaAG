package sltc

/**
 * Created by cedricbastin on 26/04/15.
 */
trait StlcSig extends agParsers.AGSig {


  def tru: Answer
  def fals: Answer

  def num(s: String): Answer
  def succ(a: Answer): Answer
  def pred(a: Answer): Answer
  def iszero(a: Answer): Answer

  def iff(a1: Answer, a2: Answer, a3: Answer): Answer

  def vari(s:String, a:Answer): Answer
  def absHead(ident:String, ty:Type): Answer
  def abs(tpe:Answer, a:Answer): Answer //Type instead of Answer
  def abs(ident:String, ty:Type, a:Answer): Answer


  def app(a1: Answer, a2: Answer): Answer
}