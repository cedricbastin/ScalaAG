package sltc

/**
 * Created by cedricbastin on 26/04/15.
 * An albegra to calculate the type from simply types lambda calculus terms
 */
trait SltcTypingAlgebra extends StlcSig {
  type Env = Map[String, Type]
  case class Answer(env: Env, tpe: Type) //only keep an environment

  def tru():Answer = Answer(Map(), TypeBool)
  def fals: Answer = Answer(Map(), TypeBool)

  def num(s: String): Answer = Answer(Map(), TypeNat)
  def succ(a: Answer): Answer = {
    if (a.tpe == TypeNat) a //stays the same
    else a.copy(tpe = WrongType("succ required Nat not: "+a.tpe))
  }
  def pred(a: Answer): Answer = {
    if (a.tpe == TypeNat) a //tpe stays the same
    else a.copy(tpe = WrongType("pred requires Nat not "+a.tpe))
  }
  def iszero(a: Answer): Answer = {
    if (a.tpe == TypeNat) a.copy(tpe = TypeBool)
    else a.copy(tpe = WrongType("iszero requires Nat not: "+a.tpe))
  }

  def iff(a1: Answer, a2: Answer, a3: Answer): Answer = {
    if (a1.tpe != TypeBool) a1.copy(tpe = WrongType("if condition should be of boolean type, not: "+a1.tpe))
    else if (a2.tpe != a3.tpe) a2.copy(tpe = WrongType("if branches should have the same type: "+a2.tpe+" 1= "+a3.tpe))
    else a2 //the same types on both branches of the if statement
  }

  def vari(s:String, a:Answer): Answer = { //answer is just passed for environment information
    a.env.get(s) match {
      case Some(t) => a.copy(tpe = t) //copy the type from the environment
      case None => a.copy(tpe = WrongType("variable name could not be found in the enviroment: "+s+a.env))
    }
  }

  //FIXME: we need the var name as well as them type after we added the tuple to the mapping
  def absHead(ident:String, ty:Type): Answer = {
    Answer(Map(ident -> ty), ty) //no env involved so far -> needs combine operator
  }

  def abs(a1:Answer, a2:Answer): Answer = {
    //FIXME: is there an easier way to do this by making "Answer" a monad?
    combine(a1, a2)
  }

  def abs(ident:String, ty:Type, ax:AnswerF): AnswerF = {
    a =>
      val body = ax(a.copy(env = a.env + (ident -> ty)))
      body.copy(tpe = TypeFun(ty, body.tpe))
  }

  def app(a1: Answer, a2: Answer): Answer = a1.tpe match {
    case TypeFun(in, out) =>
      if (in == a2.tpe) a2.copy(tpe = out) //correspond to type after function application
      else a1.copy(tpe = WrongType("in application the type of the abstraction: "+a1.tpe+" does not take an arguemnt of type: "+a2.tpe))
    case _ => a1.copy(tpe = WrongType("in application the first element is not a function: "+a1.tpe))
  }

  def combine(a1: Answer, a2: Answer): Answer = (a1, a2) match {
    case (_:WrongType, _) => a1.copy(env = a1.env ++ a2.env) //WrongTypes propagate!
    case (_, _:WrongType) => a2.copy(env = a1.env ++ a2.env)
    case _ => Answer(a1.env ++ a2.env, TypeFun(a1.tpe, a2.tpe)) //default action
  }
}
