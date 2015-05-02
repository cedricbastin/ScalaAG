package sltc

/**
 * Created by cedricbastin on 26/04/15.
 */
trait SltcTypingAlgebra extends StlcSig {
  case class Answer(env: Env, tpe: Type)
  type Env = Map[String, Type]

  def tru():Answer = Answer(Map(), TypeBool)
  def fals: Answer = Answer(Map(), TypeBool)

  def num(s: String): Answer = Answer(Map(), TypeNat) //parsed correctly as a number!
  def succ(a: Answer): Answer = {
    if (a.tpe == TypeNat) Answer(a.env, TypeNat)
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
    if (a1.tpe != TypeBool) a1.copy(tpe = WrongType("if condition should be boolean not: "+a1.tpe))
    else if (a2.tpe != a3.tpe) a2.copy(tpe = WrongType("if branches should have the same type: "+a2.tpe+" 1= "+a3.tpe))
    else a2 //the same types on both branches of the if
  }

  def vari(s:String, a:Answer): Answer = { //answer is just passed for environment information
    a.env.get(s) match {
      case Some(t) => a.copy(tpe = t) //copy the type from the environment
      case None => a.copy(tpe = WrongType("variable name could not be found in the enviroment: "+s))
    }
  }

  //FIXME: we need to pass the new environment to the abstraction body!!
  def absHead(ident:String, ty:Type): Answer = {
    Answer(Nil.toMap, ty)
  }

  def abs(a1:Answer, a2:Answer): Answer = {
    a1 //FIXME
  }

  def abs(ident:String, ty:Type, a:Answer): Answer = {
      val body = a.copy(env = a.env + (ident -> ty))
      body.copy(tpe = TypeFun(ty, body.tpe))
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
}
