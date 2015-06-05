package stlc

/**
 * Created by cedricbastin on 26/04/15.
 * An albegra to calculate the type from simply types lambda calculus terms
 */
trait SltcTypingAlgebra extends StlcSig {
  type InAttrs =  Map[String, Type]
  type SynAttr = Type
  //case class Answer(env: Env, tpe: Type) //only keep an environment

  def tru: SynAttr = TypeBool
  def fals: SynAttr = TypeBool

  def num(s: String): SynAttr = TypeNat
  def succ(a: SynAttr): SynAttr = {
    if (a == TypeNat) a //stays the same
    else WrongType("succ required Nat not: "+a)
  }
  def pred(a: SynAttr): SynAttr = {
    if (a == TypeNat) a //tpe stays the same
    else WrongType("pred requires Nat not "+a)
  }
  def iszero(a: SynAttr): SynAttr = {
    if (a == TypeNat) TypeBool
    else WrongType("iszero requires Nat not: "+a)
  }

  def iff(a1: SynAttr, a2: SynAttr, a3: SynAttr): SynAttr = {
    if (a1 != TypeBool) WrongType("if condition should be of boolean type, not: "+a1)
    else if (a2 != a3) WrongType("if branches should have the same type: "+a2+" != "+a3)
    else a2 //the same types on both branches of the if statement
  }

  def vari(s:String, a:InAttrs): SynAttr = { //answer is just passed for environment information
    a.get(s) match {
      case Some(t) => t //copy the type from the environment
      case None => WrongType("variable name:"+s+" could not be found in the enviroment: "+a)
    }
  }

  def absHead(ident:String, ty:Type): (SynAttr, InAttrs) = {
    (ty, Map(ident -> ty))
  }

  def abs(a1:SynAttr, a2:SynAttr): SynAttr = {
    TypeFun(a1, a2)
  }

//  def abs(ident:String, ty:Type, ax:AnswerF): AnswerF = {
//    a =>
//      val body = ax(a.copy(env = a.env + (ident -> ty)))
//      body.copy(tpe = TypeFun(ty, body.tpe))
//  }

  def app(a1: SynAttr, a2: SynAttr): SynAttr = a1 match {
    case TypeFun(in, out) =>
      if (in == a2) out //correspond to type after function application
      else WrongType("in application the type of the abstraction: "+a1+" does not take an arguemnt of type: "+a2)
    case _ => WrongType("in application the first element is not a function: "+a1)
  }

  def combine(a1: InAttrs, a2: InAttrs): InAttrs = a1 ++ a2 //TODOD: check for  name clashes?
}
