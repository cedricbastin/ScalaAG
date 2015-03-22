package agPC

/** Two-phase inferencer, first collect constraints, then solve them. */
class TwoPhaseInferencer extends TypeInferencers {

  type Constraint = (Type, Type) //(type Var, expected Type)
  val noConstraints: List[Constraint] = Nil

  case class TypingResult(tpe: Type, c: List[Constraint]) extends Term //added extend
  type TR = TypingResult

  /** Type <code>t</code> in <code>env</code> and return its type and a
   *  constraint list.
   */
  def collect(env: Env, t: Term): TypingResult = t match {
    case tr: TR => tr

    case True | False => TypingResult(TypeBool, noConstraints)
    case Zero => TypingResult(TypeNat, noConstraints)
    
    case Var(x) =>
      val t1 : TypeScheme = lookup(env, x)
      if (t1 == null)
        throw TypeError("Unknown variable " + x)
      TypingResult(t1.instantiate, noConstraints)

    case Succ(tr:TR) =>
      println("succtr")
      TypingResult(TypeNat, (tr.tpe, TypeNat) :: tr.c)

    case Succ(t1) =>
      val TypingResult(ty, const) = collect(env, t1)
      TypingResult(TypeNat, (ty, TypeNat) :: const)

    case Pred(tr:TR) =>
      println("predtr")
      TypingResult(TypeNat, (tr.tpe, TypeNat) :: tr.c)

    case Pred(t1) =>
      val TypingResult(ty, const) = collect(env, t1)
      TypingResult(TypeNat, (ty, TypeNat) :: const)

    case IsZero(tr:TR) =>
      println("iszerotr")
      TypingResult(TypeBool, (tr.tpe, TypeNat) :: tr.c)
    
    case IsZero(t1) =>
      val TypingResult(ty, const) = collect(env, t1)
      TypingResult(TypeBool, (ty, TypeNat) :: const)

    case If(trc:TR, tr1:TR, tr2:TR) =>
      println("iftr")
      TypingResult(tr2.tpe, (trc.tpe ,TypeBool)::(tr1.tpe, tr2.tpe)::trc.c:::tr1.c:::tr2.c)

    case If(cond, t1, t2) =>
      val TypingResult(tcond, const) = collect(env, cond)
      val TypingResult(ty1, const1) = collect(env, t1)
      val TypingResult(ty2, const2) = collect(env, t2)
      TypingResult(ty2, (tcond ,TypeBool)::(ty1, ty2)::const:::const1:::const2)

    //case Abs(v:String, tp:TypeTree, t:tr) =>
      //env need to be calculated and given to the type inference of t!!!

    case Abs(v: String, tp: TypeTree, t: Term) => 
      /**If the type for abs is not specified, we create a new TypeVar*/
      val tpsch = TypeScheme(Nil, if (tp != EmptyType) toType(tp) else Type.factorFresh) 
      val TypingResult(ty, const) = collect((v,tpsch)::env, t)
      TypingResult(TypeFun(tpsch.tp, ty), const)
      
    case App(t1: Term, t2: Term) => //TAPL: p.321
      /**Constraints for t1 + Type*/
      val TypingResult(ty1, const1) = collect(env, t1)
      /**Constraints for t2 + Type*/
      val TypingResult(ty2, const2) = collect(env, t2)
      val tx = Type.factorFresh
      TypingResult(tx, (ty1,TypeFun(ty2,tx))::const1:::const2)

    case Let(x, v, t) =>
      val TypingResult(s, cstv) = collect(env, v)
      val subst = unify(cstv)
      val ty: Type = subst(s)
      val newenv = subst(env)
      val TypingResult(finaltp, cst2) = collect((x, generalize(newenv, ty)) :: newenv, t)
      TypingResult(finaltp, cstv:::cst2) //keep track of the constraints of the left-hand side!
  }
  
  /**We generalize the type variables in T as long as they do not belong
   * to the environment*/
  def generalize(e : Env, tp : Type) : TypeScheme = {
    def polyGen(ty : Type) : List[TypeVar] = ty match {
       case TypeBool | TypeNat => Nil
       case TypeFun(a, b) => polyGen(a) ::: polyGen(b)
       case tv @ TypeVar(x) if e.map(_._2.tp).contains(tv) => Nil //the variable cannot be generalized because it is not the one we just added
       case TypeVar(x) => TypeVar(x) :: Nil
       case terror => throw new TypeError("In generalize : type "+terror+" unknown")
     }
    /**Create the generalized type scheme*/
    TypeScheme(polyGen(tp), tp)
  }
 
  /**
   */
  def unify(c: List[Constraint]): Substitution ={
    if (c.isEmpty) emptySubst
    else c.head match {
      case (a, b) if (a == b) =>
        unify(c.tail)
      case (tv @ TypeVar(x), tp : Type) if (!tp.collectTV.contains(x)) => 
        unify(c.tail.map{x => if ((x._1.collectTV union x._2.collectTV) contains tv.name) emptySubst.extend(tv, tp)(x) else x}).extend(tv,tp)
       
      /**the other way around: the TV is on the right */
      case (tp, tv @ TypeVar(x)) if (! tp.collectTV.contains(x)) => 
        unify(c.tail.map(emptySubst.extend(tv, tp).apply(_))).extend(tv, tp) 
        
      /**If it is a function we have to unify the args and the result types separately*/  
      case (TypeFun(a, b), TypeFun(d,e)) =>
         unify(c.tail:::((a,d)::(b,e)::Nil))
      
      case (t1, t2) =>
        throw TypeError("Could not unify: " + t1 + " with " + t2)
    }
  }

  def typeOfTR(tr: TypingResult): Type = {
    val s = unify(tr.c)
    s(tr.tpe)
  }

  override def typeOf(t: Term): Type = try {
    val TypingResult(tp, c) = collect(Nil: Env, t) //collect contraints
    val s = unify(c) 
    s(tp)
  } catch {
    case TypeError(msg) =>
      Console.println("type error: " + msg)
      null
  }

}
