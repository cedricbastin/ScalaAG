package sltc

import scala.util.parsing.input.Positional

/** Abstract Syntax Trees for terms. */
abstract class Term extends Positional

case object True extends Term {
  override def toString() = "true"
}

case object False extends Term {
  override def toString() = "false"
}
case object Zero extends Term {
  override def toString() = "0"
}

case class IsZero(t: Term) extends Term {
  override def toString = "iszero "+t
}

case class If(i: Term, t: Term, e: Term) extends Term {
  override def toString = "if "+i+" then "+t+" else "+e
}

/*Numerical Value*/
case class Succ(t: Term) extends Term {
  override def toString = "succ "+t
}

case class Pred(t: Term) extends Term {
  override def toString = "pred "+t
}

case class Var(ident: String) extends Term {
  override def toString = ident
}
/*Represents an abstraction. As intended, the variable has to be typed*/
case class Abstraction(x: Var, xt: TypeTree, t: Term) extends Term {
  override def toString = "(\\"+x+":"+xt+"."+t+")"
}
case class Application(t1: Term, t2: Term) extends Term {
  override def toString = (t1,t2) match {
    case (_ , _ :Application) => t1+" ("+t2+")"	/*Protect right application*/
    case _ => t1+" "+t2
  }
}

/** Abstract Syntax Trees for types. */
abstract class TypeTree extends Term

case object BoolType extends TypeTree {
  override def toString() = "Bool"
}

/*Type for natural numbers*/
case object NatType extends TypeTree {
  override def toString = "Nat"
}

case class FunType(t1: TypeTree, t2: TypeTree) extends TypeTree {
  override def toString = (t1,t2) match {
    case (_: FunType, _) => "("+t1+")->"+t2 //protect left association because right associative by default
    case _ => t1+"->"+t2
  }
}

case object EmptyType extends TypeTree


//real types
abstract class Type {
  def collectTV: Set[String]
  override def toString():String
}

case class TypeVar(name: String) extends Type {
  def collectTV = Set(name)
  override def toString = name
}
case class TypeFun(in : Type, out : Type) extends Type {
  def collectTV = in.collectTV union out.collectTV
  override def toString = "(" + in + " -> " + out + ")"
}

case object TypeNat extends Type {
  def collectTV = Set.empty[String]
  override def toString = "Nat"
}
case object TypeBool extends Type {
  def collectTV = Set.empty[String]
  override def toString = "Bool"
}

case object TypeUnit extends Type {
  def collectTV = Set.empty[String]
  override def toString = "Unit"
}

case class WrongType(msg: String) extends Type {
  def collectTV = Set.empty[String]
  override def toString = "Wrong type: "+msg
}

/** Type Schemes are not types. */
case class TypeScheme(args: List[TypeVar], tp: Type) {
  /**Gives a fresh name for all variables in args that are in tp*/
  def mapFresh =((args.map(_.name).toSet intersect tp.collectTV).map(TypeVar(_))).map(x => (x, Type.factorFresh)).toMap

  /**We need to get a fresh type for each argument in args that are present in tp*/
  def instantiate:Type = {
    def recInstant(tpe : Type, map : Map[TypeVar, TypeVar]) : Type= tpe match {
      case tv : TypeVar => map.getOrElse(tv,tv)
      case TypeFun(x,y) => TypeFun(recInstant(x, map), recInstant(y, map))
      case _ => tpe
    }
    /**Call the recursive method with a unique fresh map*/
    recInstant(tp, mapFresh)
  }
  override def toString() = args.mkString("[", ", ", "].") + tp
}

object Type {
  private var id : Int = -1

  /**Returns a fresh name for a new type Variable*/
  def factorFresh  = {this.id +=1; TypeVar("T"+id)}

}

abstract class Substitution extends (Type => Type) {

  def apply(tp: Type): Type = {
    val result:Type = tp match {
      case tv:TypeVar => lookup(tv)
      case TypeFun(a,b) => TypeFun(this(a), this(b))
      case TypeNat | TypeBool => tp
    }

    result
  }

  override def toString() = ""

  def apply(p: (Type, Type)): (Type, Type) = p match {
    case Pair(t1, t2) => (this(t1), this(t2))
  }

  def apply(env: List[(String, TypeScheme)]): List[(String, TypeScheme)] =
    env map { (pair) => (pair._1, TypeScheme(pair._2.args, apply(pair._2.tp))) }

  def lookup(tv: TypeVar):Type

  /**Enables to compose substitutions, that is, applying one substitution after the other
    * We implemented it by applying returning a substitution that applies both original substs*/
  def extend(tv : TypeVar, tp: Type):Substitution = new Substitution { //TAPL: p.318
  def lookup(tt: TypeVar) = if (tt == tv) tp else Substitution.this.lookup(tt)
    override def toString = Substitution.this+"["+tv+"->"+tp+"]"
  }
}

/** The empty substitution. */
object emptySubst extends Substitution {
  def lookup(t: TypeVar) = t
  override def toString() = "EmptySubst"
}
