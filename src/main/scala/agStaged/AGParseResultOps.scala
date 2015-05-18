package agStaged

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericCodegen

import stagedparsec.ReaderOps
import lms._
//import lms.util._
import scala.reflect.SourceContext

/**
* Created by cedricbastin on 11/05/15.
*/
trait AGParseResultOps extends AGSig with Base with IfThenElse with BooleanOps {
  self: ReaderOps =>

  abstract class AGParseResult[+T: Manifest] { //manifests need to be forwarded from the first call where they will be generated implicitly
    def isEmpty: Rep[Boolean]
    def next: Rep[Input] //Input defined by ReaderOps
    def res: Rep[T]
    def ans: Rep[Answer]
  }

  //abstract Rep[_] world
  def AGSuccess[T: Manifest](res: Rep[T], next: Rep[Input], ans: Rep[Answer]): Rep[AGParseResult[T]]
  def AGFailure[T: Manifest](next: Rep[Input]): Rep[AGParseResult[T]] //[Nothing]

  //pimp my library pattern -> implicit converter
  implicit class ParseResultCls[A: Manifest](pr: Rep[AGParseResult[A]]) {
    def isEmpty: Rep[Boolean] =
      parseresult_isEmpty(pr)
    def get: Rep[A] =
      parseresult_get(pr)
    def orElse(that: Rep[AGParseResult[A]]) =
      parseresult_orElse(pr, that)
    def next: Rep[Input] =
      parseresult_next(pr)
    def ans: Rep[Answer] =
      parseresult_ans(pr)

    def map[B: Manifest](f: Rep[A] => Rep[B]) = parseresult_map(pr, f)
    //other fucntions
  }

  def parseresult_isEmpty[A: Manifest](pr: Rep[AGParseResult[A]]) (implicit pos: SourceContext): Rep[Boolean]
  def parseresult_get[A: Manifest](pr: Rep[AGParseResult[A]])     (implicit pos: SourceContext): Rep[A]
  def parseresult_next[A: Manifest](pr: Rep[AGParseResult[A]])    (implicit pos: SourceContext): Rep[Input]
  def parseresult_ans[A: Manifest](pr: Rep[AGParseResult[A]])     (implicit pos: SourceContext): Rep[Answer]

  def parseresult_orElse[A: Manifest](pr: Rep[AGParseResult[A]], that: Rep[AGParseResult[A]])(implicit pos: SourceContext): Rep[AGParseResult[A]] =
    if (pr.isEmpty) that else pr

  def parseresult_map[A: Manifest, B: Manifest](pr: Rep[AGParseResult[A]], f: Rep[A] => Rep[B])(implicit pos: SourceContext): Rep[AGParseResult[B]] =
    if (pr.isEmpty) AGFailure[B](pr.next) else AGSuccess(f(pr.get), pr.next, pr.ans)

}

//concrete Exp world
trait AGParseResultOpsExp extends AGParseResultOps with IfThenElseExp with BooleanOpsExp with StructOpsExpOpt with CastingOpsExp {
  self: ReaderOps =>

  def parseresult_isEmpty[A: Manifest](pr: Rep[AGParseResult[A]])(implicit pos: SourceContext): Rep[Boolean] =
    field[Boolean](pr, "empty")

  def parseresult_get[A: Manifest](pr: Rep[AGParseResult[A]])(implicit pos: SourceContext): Rep[A] =
    field[A](pr, "res")

  def parseresult_next[A: Manifest](pr: Rep[AGParseResult[A]])(implicit pos: SourceContext): Rep[Input] =
    field[Input](pr, "next")

  def parseresult_ans[A: Manifest](pr: Rep[AGParseResult[A]])(implicit pos: SourceContext): Rep[Answer] =
    field[Answer](pr, "ans")

  def AGSuccess[T: Manifest](res: Rep[T], next: Rep[Input], ans: Rep[Answer]): Exp[AGParseResult[T]] =
    struct(
      classTag[AGParseResult[T]],
      "res" -> res,
      "empty" -> unit(false),
      "next" -> next,
      "ans" -> ans
    )

  def AGFailure[T: Manifest](next: Rep[Input]): Exp[AGParseResult[T]] =
    struct(
      classTag[AGParseResult[T]],
      "res" -> unit(ZeroVal[T]),
      "empty" -> unit(true),
      "next" -> next,
      "ans" -> unit(ZeroVal[Answer]) //TODO: is this needed? no manifest available for answer as it is a abstract type
    )
}

trait AGParseResultGenBase extends GenericCodegen with BaseGenStructOps {
  val IR: AGParseResultOpsExp

  override def remap[A](m: Manifest[A]) = m.erasure.getSimpleName match {
    case "ParseResult" => IR.structName(m)
    case _ => super.remap(m)
  }
}

trait ScalaGenParseResultOps
  extends ScalaGenBase
  with AGParseResultGenBase
  with ScalaGenStructOps
  with ScalaGenIfThenElse
  with ScalaGenBooleanOps {
  val IR: AGParseResultOpsExp
}